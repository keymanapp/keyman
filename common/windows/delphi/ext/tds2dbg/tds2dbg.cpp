// Copyright (c) 2009-2010, Bjarne Juul Pasgaard
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice, this permission notice and the following disclaimer
// appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

#include "tds2dbg.h"
#include "tdscvstructs.h"
#include "log.h"
#include "fileutils.h"
#include <string>
#include <map>
#include <stdexcept>
#include <iomanip>


/// @brief Ensures that the next write operation starts on an aligned
///        address.
///
/// @param dst The target to write to.
/// @param p   The current write position of the target.
/// @param m   Alignment mask. Must be of the form 2^n-1.
///
/// @tparam T The target (e.g. file wrapper) class to write to. It must
///           provide a <pre>void write(unsigned char*, unsigned int);</pre>
///           method.
///
/// @throw runtime_error In case of errors.
template <class T>
void zeroPad(T& dst, unsigned int p, unsigned int m) throw(std::runtime_error)
{
   unsigned int algnmnt = p & m;
   if(algnmnt) {
      unsigned char filler = 0;
      dst.writeRep(filler,1+m-algnmnt);
   }
}

/// @brief Ensure that a symbol record to be written does not cross a
///        page boundary in a file.
///
/// S_ALIGN symbols are inserted to ensure proper page alignment.
///
/// @throw runtime_error In case of errors.
void
pageAlignSymbol(FileWrapper& f, unsigned int len, unsigned int mask)
   throw(std::runtime_error)
{
   unsigned int headPg = (f.tell())     & (~mask);
   unsigned int tailPg = (f.tell()+len) & (~mask);
   if(headPg != tailPg) {
      // Compute the number of bytes to skip
      tailPg -= f.tell();

      // Enough bytes for a S_ALIGN record with 4 bytes padding?
      if(tailPg<8) {
         // Otherwise, skip a complete page.
         tailPg += 1 + mask;

      }
      // Report on alignment to be applied
      mylog()(4) << "    Inserted S_ALIGN at " << f.tell()
                 << " with " << tailPg-4
                 << " padding bytes to make room for "
                 << len << " bytes." << std::endl;

      CVsymRecHdr symHdr(tailPg-2,CVskAlign);

      f.write(symHdr);

      unsigned char pad = 0;

      f.writeRep(pad,tailPg-4);

   }
}

/// @brief Ensure that a block of bytes to be written does not cross a
///        page boundary in a file.
///
/// 0's are inserted to ensure proper page alignment.
///
/// @throw runtime_error In case of errors.
void
pageAlign(FileWrapper& f, unsigned int len, unsigned int mask)
   throw(std::runtime_error)
{
   unsigned int headPg = (f.tell())     & mask;
   unsigned int tailPg = (f.tell()+len) & mask;
   if(headPg != tailPg) {
      // Compute the number of bytes to skip
      tailPg -= f.tell();

      unsigned char pad = 0;

      f.write(pad,tailPg);
   }
}

/// @brief Handles conversion (from TDS to Code View) of and settings related
///        to various elements.
class Converter
{
public:

   /// @brief Constructor.
   ///
   /// @param doConvTypes      True if types are to be converted.
   /// @param staticsAsPublics True if statics are to be treated as publics.
   Converter(bool doConvTypes, bool staticsAsPublics) :
      m_doConvTypes(doConvTypes), m_staticsAsPublics(staticsAsPublics) {}

   /// @brief Converts from a 4 byte TDS type index to a 2-byte DBG type index.
   ///
   /// @param typeIdx  The 4-byte TDS type index.
   ///
   /// @return The 2-byte DBG type index.
   WORD typIdx(DWORD typeIdx) const;

   /// @brief Loads all names from the sstNames subsection in a
   ///        TDS file into an internal cache.
   ///
   /// @param tdsFile  The TDS file to read from.
   /// @param offs     Location of the sstNames subsection in the tds file.
   /// @param sz       Number of bytes in the sstNames subsection.
   ///
   /// @throw runtime_error If reading of sstNames failed.
   void loadNameCache(FileWrapper& tdsFile, unsigned int offs, unsigned int sz)
      throw(std::runtime_error);

   /// @brief Returns a name with a specified 1-based index.
   ///
   /// @param nameIdx  The 1-based index of the name.
   ///
   /// @return The name retrieved. Empty if not found.
   std::string getName(unsigned int nameIdx) const;

   /// @brief Returns true if sstGlobalTypes is to be converted.
   ///
   /// @retval true  If sstGlobalTypes is to be converted.
   /// @retval false Otherwise.
   bool doConvertTypes() const { return m_doConvTypes; }

   /// @brief Returns true if statics are to be treated as publics.
   ///
   /// @retval true  If statics are to be treated as publics.
   /// @retval false Otherwise.
   bool staticAsPub() const { return m_staticsAsPublics; }

protected:

   /// @brief True if types are to be converted.
   bool      m_doConvTypes;

   /// @brief True if statics are to be treated as publics
   bool      m_staticsAsPublics;

   /// @brief Name cache container type.
   typedef std::map<unsigned int,std::string> NameCache;

   /// @brief Cache of names.
   ///
   /// The key is the name index
   NameCache m_cache;

};

WORD
Converter::typIdx(DWORD typeIdx) const {
   // TypeIdx lower than 0x1000 (predefined types) do not need conversion.
   if(!m_doConvTypes && typeIdx>=0x1000) {
      return 0;
   }

   static bool warned = false;

   if(typeIdx>0xFFFF) {
      if(!warned) {
         mylog()(1)
            << "Warning: TDS type index too large for DBG file" << std::endl;
         warned = true;
      }
      return 0;
   }

   return (WORD) typeIdx;
}

void
Converter::
loadNameCache(FileWrapper& tdsFile, unsigned int offs, unsigned int sz)
   throw(std::runtime_error)
{
   // Remember the current read position
   unsigned int oldPos = tdsFile.tell();


   // Read the number of names
   unsigned int numNames;
   tdsFile.seek(offs);
   tdsFile.read(numNames);

   // Loop to read in all names
   unsigned int  nameIdx = 1;

   while(sz>4 && numNames) {
      // Read length of the name
      unsigned char len;
      tdsFile.read(len);

      // Prepare a slot in the name cache
      std::pair<NameCache::iterator,bool> insertPoint =
         m_cache.insert(NameCache::value_type(nameIdx++,std::string()));
      insertPoint.first->second.clear();
      insertPoint.first->second.reserve(len);

      // Loop to read the contents of the name
      while(sz && len) {
         unsigned char c;
         tdsFile.read(c);
         insertPoint.first->second.push_back(c);
         --sz;
         --len;
      }

      // An additional 0 is included between each name. Skip it!
      if(sz) {
         tdsFile.skip(1);
         --sz;
      }

      // Next name
      --numNames;
   }

   // Return to the old read position
   tdsFile.seek(oldPos);
}

std::string
Converter::getName(unsigned int nameIdx) const
{
   static bool warningReported = false;

   NameCache::const_iterator it = m_cache.find(nameIdx);

   if(m_cache.end() != it) {
      return it->second;
   } else if(!warningReported) {
      warningReported = true;
      mylog()(1) << "Failed to get name with index "
                 << std::hex << nameIdx << ". Further reports suppressed."
                 << std::endl;
   }
   return std::string();
}

/// @brief The number of hash buckets in the name hash table.
#define NUM_HASH_BUCKETS 0x82

/// @brief Internal symbol name hash table
class NameHshTbl
{
public:

   /// @brief Constructor.
   ///
   /// @param numBuckets The number of buckets to use in the hash table.
   NameHshTbl(unsigned short numBuckets);

   /// @brief Appends a symbol to the name hash table.
   ///
   /// @param n      The name of the symbol to append.
   /// @param offset The offset of the symbol reference from the beginning of
   ///               the symbols.
   void append(const std::string& n, DWORD offset);

   /// @brief Writes the symbol name hash table to a symbol subsection of a
   ///        DBG file.
   ///
   /// @param      dbgFile    The DBG file to write to.
   /// @param[out] hshTableSz The size of the generated address table.
   ///
   /// @throw runtime_error In case of errors.
   void write(FileWrapper& dbgFile, DWORD& hshTableSz) const
      throw(std::runtime_error);

protected:

   /// @brief Element in a hash bucket.
   struct HshBcktElem
   {
      /// @brief Default constructor.
      HshBcktElem() {}

      /// @brief Constructor.
      ///
      /// @param offs  The offset of the symbol reference from the beginning of
      ///              the symbols.
      /// @param hsh   The name hash of the symbol.
      HshBcktElem(DWORD offs, DWORD hsh) : symRefOffs(offs), nameHash(hsh) {}

      /// @brief The offset of the symbol reference from the beginning of the
      ///        symbols.
      DWORD symRefOffs;

      /// @brief The name hash of the symbol.
      DWORD nameHash;
   };

   /// @brief Container type for the hash buckets that make up the table.
   ///
   /// The index is the hash bucket number and the value is a vector of
   /// symbol offsets in that bucket.
   typedef std::vector<std::vector<HshBcktElem> > HashBuckets;

protected:

   /// @brief The internal name hash table.
   HashBuckets hshTable;

};

NameHshTbl::NameHshTbl(unsigned short numBuckets) : hshTable(numBuckets)
{
}

void
NameHshTbl::write(FileWrapper& dbgFile, DWORD& hshTableSz) const
   throw(std::runtime_error)
{
   // Remember where we start writing to
   unsigned int strtPos = dbgFile.tell();
   
   // Write out the number of hash buckets.
   unsigned short numBuckets = hshTable.size();
   dbgFile.write(numBuckets);

   // Filler bytes
   numBuckets = 0;
   dbgFile.write(numBuckets);

   // Write out offsets of each buckets offset table
   unsigned int dw = 0;
   HashBuckets::const_iterator bcktIt = hshTable.begin();
   while(hshTable.end() != bcktIt) {
      // Write out current offset
      dbgFile.write(dw);
      // Compute offset of next buckets offset table
      dw += bcktIt->size() * 8;
      // Next bucket
      ++bcktIt;
   }

   // Write out the number of elements in each buckets offset table
   bcktIt = hshTable.begin();
   while(hshTable.end() != bcktIt) {
      // Write out the number of elements in this buckets offset table
      dw = bcktIt->size();
      dbgFile.write(dw);
      // Next bucket
      ++bcktIt;
   }

   // Write out the offset tables for all buckets
   bcktIt = hshTable.begin();
   while(hshTable.end() != bcktIt) {
      // Loop over all symbols within this bucket
      std::vector<HshBcktElem>::const_iterator symIt = bcktIt->begin();
      while(bcktIt->end() != symIt) {
         // Write out offset and name hash of current symbol
         dbgFile.write(*symIt);
         // Next symbol
         ++symIt;
      }
      // Next segment
      ++bcktIt;
   }

   // Compute the total size of the address hash table
   hshTableSz = dbgFile.tell() - strtPos;

   // Log the results
   mylog()(2) << " Appended name hash table at " << std::hex
              << strtPos << " with size " << hshTableSz << std::endl;
}

void NameHshTbl::append(const std::string& n, DWORD offset)
{
   unsigned int  l = n.size();
   unsigned long hshTail = 0;
   while(l & 3) {
      --l;
      hshTail  |= (n[l] & 0xDF);
      hshTail <<= 8;
   }

   l >>= 2;

   unsigned long hsh = 0;
   const unsigned long* ln =
      reinterpret_cast<const unsigned long*>(n.c_str());
   for(unsigned int i=0; i < l; ++i) {
      hsh ^= 0xDFDFDFDF & (ln[i]);
      _lrotl(hsh,4);
   }

   hsh ^= hshTail;

   hshTable[hsh % hshTable.size()].
      push_back(HshBcktElem(offset,hsh));
}

/// @brief Record used internally for temporary storage of symbols.
struct SymbolRecord {
   DWORD typeIdx;      ///< Type index of the symbol.
   DWORD nameIdx;      ///< Name index of the symbol.
   WORD  symKind;      ///< Symbol kind. 
   WORD  moduleIdx;    ///< Index of module that contains this symbol record.
   DWORD offsSymRec;   /**< Offset of symbol record in the DBG symbol table
                            where it is located. */
   DWORD symRefOffs;   ///< Symbol reference offset to insert in hash table. */
};

/// @brief Container type of the temporary storage of symbols within a segment.
///
/// The key is the offset relative to the segment. This ensures that
/// symbols are ordered according to the offset.
typedef std::map<DWORD,SymbolRecord> SegmSymMap;

/// @brief Container type of the temporary storage of public symbols.
///
/// The key is the segment.
typedef std::map<WORD,SegmSymMap> SymRecMap;

/// @brief Appends an address hash table to a symbol subsection.
///
/// @param      dbgFile    The DBG file to write to.
/// @param[out] hshTableSz The size of the generated address table.
/// @param      syms       The symbols to generate the hash table for.
///                        The symRefOffs member for each symbol must
///                        have been set correctly.
///
/// @throw runtime_error In case of errors.
void createAddrHashTable(FileWrapper&     dbgFile,
                         DWORD&           hshTableSz,
                         const SymRecMap& syms) throw(std::runtime_error)
{
   // Remember where we start writing to
   unsigned int strtPos = dbgFile.tell();

   // Write out the number of segments.
   unsigned short numSegs = syms.size();
   dbgFile.write(numSegs);

   // Filler bytes
   numSegs = 0;
   dbgFile.write(numSegs);

   // Write out offsets of each segments offset table
   unsigned int dw = 0;
   SymRecMap::const_iterator segIt = syms.begin();
   while(syms.end() != segIt) {
      // Write out current offset
      dbgFile.write(dw);
      // Compute offset of next segments offset table
      dw += segIt->second.size() * 8;
      // Next segment
      ++segIt;
   }

   // Write out the number of elements in each segments offset table
   segIt = syms.begin();
   while(syms.end() != segIt) {
      // Write out the number of elements in this segments offset table
      dw = segIt->second.size();
      dbgFile.write(dw);
      // Next segment
      ++segIt;
   }

   // Write out the offset tables for all segments
   segIt = syms.begin();
   while(syms.end() != segIt) {
      // Loop over all symbols within this segment
      SegmSymMap::const_iterator symIt = segIt->second.begin();
      while(segIt->second.end() != symIt) {
         // Write out offset of symbol record within the subsection
         dbgFile.write(symIt->second.symRefOffs);
         // Write out offset of symbol relative to segment
         dbgFile.write(symIt->first);
         // Next symbol
         ++symIt;
      }
      // Next segment
      ++segIt;
   }

   // Compute the total size of the address hash table
   hshTableSz = dbgFile.tell() - strtPos;

   // Log the results
   mylog()(2) << " Appended address hash table at " << std::hex
              << strtPos << " with size " << hshTableSz << std::endl;
}

/// @brief Appends a closing S_ALIGN symbol.
///
/// @param dbgFile The file to append to.
/// @param algnmtn The alignment mask to apply.
///
/// @throw runtime_error In case of errors.
void appendClosingAlign(FileWrapper& dbgFile) throw(std::runtime_error)
{
   // The symbol record header
   CVsymRecHdr recHdr(6,CVskAlign);

   // Reserve space for a DBG symbol record header to be
   // written out later.
   HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

   // Create a record writer to keep track of the number of
   // payload bytes written.
   RecordWriter wrtr;

   DWORD pad = 0xFFFFFFFF;

   wrtr.write(pad);

   // Ensure that the record does not cross a page boundary.
   unsigned int bytesToWrite = dbgRecHdr.length(wrtr);
   pageAlignSymbol(dbgFile,bytesToWrite,0xFFF);

   dbgRecHdr.commit(dbgFile,wrtr);
}

/// @brief Converts a TDS symbol table to a DBG symbol table.
///
/// See "Symbols" in Tools Interface Standard Formats Specification
/// for Windows version 1.0 for the definition of a DBG type record
/// and type string.
///
/// @param      conv           Converter used for various conversion tasks.
/// @param      moduleIdx      Index of the current module. 0 if none.
/// @param      tdsFile        The input tds file.
/// @param      tdsTableSz     The size of the symbol table in the input.
/// @param      dbgFile        The output dbg file.
/// @param[out] symTableSz     The size of the symbol table in the output.
/// @param[out] syms           Addressable symbols already and inserted in
///                            the generated symbol table.
/// @param[out] pubSyms        Contents for the sstGlobalPub subsection.
/// @param[out] staticSyms     Contents for the sstStaticSym subsection.
///
/// @throw runtime_error In case of errors.
void convertSymbolSubsection(const Converter&  conv,
                             unsigned short    moduleIdx,
                             FileWrapper&      tdsFile,
                             DWORD             tdsTableSz,
                             FileWrapper&      dbgFile,
                             DWORD&            symTableSz,
                             SymRecMap&        syms,
                             NameHshTbl&       nameHshTable,
                             SymRecMap&        pubSyms,
                             SymRecMap&        staticSyms)
   throw(std::runtime_error)
{
   // Remember where we start writing to
   unsigned int strtPos = dbgFile.tell();

   // Loop over all symbol records in the TDS file
   RecordReader rdr(tdsFile,tdsTableSz);

   while(!rdr.eor()) {

      // Remember the file pos that we will start reading the record from
      unsigned int tdsHdrPos = rdr.tell();

      // Read the symbol record header
      CVsymRecHdr recHdr;
      rdr.read(recHdr);

      // Remember the record length
      unsigned int recLen = recHdr.len;

      switch(recHdr.symKind) {
      case CVskCompile: {
         // Handle compile flags (copy almost as is --- seems like the
         // tds files have a wrong length field, which we must correct).
         mylog()(3) << "   Converting S_COMPILE" << std::endl;

         // Read the payload header from the tds file.
         CVsrCompileFlagPayloadHdr tdsPlHdr;
         rdr.read(tdsPlHdr);

         BYTE versionLen;
         rdr.read(versionLen);

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of
         // payload bytes written.
         RecordWriter wrtr;

         // Write the payload header from the TDS file as is.
         wrtr.write(tdsPlHdr);

         // Write a length-prefixed name (copy from TDS file).
         wrtr.write(versionLen);
         copyFileContent(wrtr,tdsFile,versionLen);

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);
         break;
      }
      case CVskRegister: {
         // Handle Register symbol record
         mylog()(3) << "   Converting S_REGISTER" << std::endl;

         // Read the symbol record payload from the TDS file
         TDSsrRegisterPayload tdsPl;
         rdr.read(tdsPl);

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of payload
         // bytes written.
         RecordWriter wrtr;

         // Create the payload header for the DBG file
         CVsrRegisterPayloadHdr dbgPlHdr = {
            conv.typIdx(tdsPl.typeIdx),
            tdsPl.regId
         };

         // Write the payload header
         wrtr.write(dbgPlHdr);

         // Append the length-prefixed name of the symbol to the payload
         wrtr.write1bLpfName(conv.getName(tdsPl.nameIdx));

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);
         break;
      }
      case CVskUDT: {
         // Handle a User defined type

         if(conv.doConvertTypes()) {

            mylog()(3) << "   Converting S_UDT" << std::endl;

            // Read the entire payload from the TDS file
            TDSsrUserDefinedTypePayload tdsPl;
            rdr.read(tdsPl);

            // If sstGlobalTypes is not converted, we can only handle
            // predefined types (typeIdx<0x1000).

            // Reserve space for a DBG symbol record header to be
            // written out later.
            HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

            // Create a record writer to keep track of the number of
            // payload bytes written.
            RecordWriter wrtr;

            // Create the payload header for the DBG file
            CVsrUserDefinedTypePayloadHdr dbgPlHdr =
               {conv.typIdx(tdsPl.typeIdx)};

            // Write the payload header
            wrtr.write(dbgPlHdr);

            // Get the type name
            const std::string& tpName = conv.getName(tdsPl.nameIdx);

            // Append the length-prefixed type name to the payload
            wrtr.write1bLpfName(tpName);

            // Ensure that the next record is properly aligned on a
            // dword boundary
            zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

            // Ensure that the record does not cross a page boundary.
            pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

            // The type name should potentially be referenced from a
            // name hash table, so register it's location in the name hash.
            nameHshTable.append(tpName,dbgFile.tell()-strtPos);

            // Write the symbol record header with the correct
            // length field
            dbgRecHdr.commit(dbgFile,wrtr);

         } else {
            mylog()(3) << "   Skipping S_UDT" << std::endl;
         }
         break;
      }
      case CVskSSearch: {
         // Handle a Start Search Symbol
         mylog()(3) << "   Converting S_SSEARCH" << std::endl;

         // Read the payload from the TDS file
         TDSsrStartSearchPayload tdsPl;
         rdr.read(tdsPl);

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of
         // payload bytes written.
         RecordWriter wrtr;

         // Create the corresponding DBG payload
         CVsrStartSearchPayload dbgPl = {tdsPl.offs, tdsPl.segment};

         // Write the payload to the DBG file
         wrtr.write(dbgPl);

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);
         break;
      }
      case CVskEnd: {
         // Handle an End of Block symbol
         // No payload, no conversion, just write as is.
         mylog()(3) << "   Converting S_END" << std::endl;

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of
         // payload bytes written (none).
         RecordWriter wrtr;

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);
         
         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);
         break;
      }
      case CVskBPRel: {
         // Handle a BP Relative symbol
         mylog()(3) << "   Converting S_BPREL" << std::endl;

         // Read the payload from the TDS file
         TDSsrBPRelativePayload tdsPl;
         rdr.read(tdsPl);

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of
         // payload bytes written.
         RecordWriter wrtr;

         // Create the corresponding payload header for the DBG file
         CVsrBPRelativePayloadHdr dbgPlHdr = {
            tdsPl.offset,conv.typIdx(tdsPl.typeIdx)
         };

         // Write the payload header to the DBG file
         wrtr.write(dbgPlHdr);

         // Append the length-prefixed symbol name to the payload header
         wrtr.write1bLpfName(conv.getName(tdsPl.nameIdx));

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);
         break;
      }
      case CVskLocalData: // Intentional fall-through
      case CVskGlobalData: {

         if(CVskLocalData == recHdr.symKind) {
            mylog()(3) << "   Converting S_LDATA" << std::endl;
         } else {
            mylog()(3) << "   Converting S_GDATA" << std::endl;
         }

         // Read the payload from the TDS file
         TDSsrGlobalDataPayload tdsPl;
         rdr.read(tdsPl);

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of
         // payload bytes written.
         RecordWriter wrtr;

         // Create the corresponding payload header for the DBG file
         CVsrGlobalDataPayloadHdr dbgPlHdr =
            {tdsPl.offset,tdsPl.segment,conv.typIdx(tdsPl.typeIdx)};

         // Write the payload header to the DBG file
         wrtr.write(dbgPlHdr);

         // Get the symbol name
         const std::string& symName = conv.getName(tdsPl.nameIdx);

         // Append the length-prefixed symbol name to the payload header
         wrtr.write1bLpfName(symName);

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

         // Remember where the record is written to
         unsigned int dbgHdrPos = dbgFile.tell() - strtPos;

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);

         // Global and Local Data symbol records must have a
         // corresponding PUB32 record in the sstGlobalPub
         // section or a corresponding DATAREF record in the
         // sstStaticSym, respectively, of the DBG file as
         // well. We must therefore store the record for later
         // use.
         SymbolRecord sr;

         sr.typeIdx    = tdsPl.typeIdx;
         sr.nameIdx    = tdsPl.nameIdx;
         sr.moduleIdx  = moduleIdx;
         sr.offsSymRec = dbgHdrPos;
         sr.symRefOffs = dbgHdrPos;

         if(CVskGlobalData == recHdr.symKind || conv.staticAsPub()) {
            sr.symKind = CVskPublic;
            pubSyms[tdsPl.segment][tdsPl.offset] = sr;
         } else {
            sr.symKind = CVskDataRef;
            staticSyms[tdsPl.segment][tdsPl.offset] = sr;
         }

         // The address of the symbol should potentially be referenced
         // from an address hash table, so we need to add to the syms.
         syms[tdsPl.segment][tdsPl.offset] = sr;

         // The symbol name should potentially be referenced from a
         // name hash table, so we need to register it in the name hash.
         nameHshTable.append(symName,dbgHdrPos);

         break;
      }
      case CVskPublic: // Intentional fall-through
      case CVskLocalProc:
      case CVskGlobalProc: {
         // Handle Public, LocalProc, or GlobalProc symbol

         if(CVskPublic == recHdr.symKind) {
            mylog()(3) << "   Converting S_PUB" << std::endl;
         } else if(CVskLocalProc == recHdr.symKind) {
            mylog()(3) << "   Converting S_LPROC" << std::endl;
         } else {
            mylog()(3) << "   Converting S_GPROC" << std::endl;
         }

         // Read the payload header from the TDS file (ignore the
         // length-prefixed linker name of the symbol).
         TDSsrLocalProcPayloadHdr tdsPlHdr;
         rdr.read(tdsPlHdr);

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of
         // payload bytes written.
         RecordWriter wrtr;

         // Prepare the payload header for the DBG file.
         CVsrLocalProcPayloadHdr dbgSrHdr =
            {tdsPlHdr.parent,tdsPlHdr.end,tdsPlHdr.next,
             tdsPlHdr.procLen,tdsPlHdr.dbgStart,tdsPlHdr.dbgEnd,
             tdsPlHdr.offset,tdsPlHdr.segment,
             conv.typIdx(tdsPlHdr.typeIdx), (BYTE) (tdsPlHdr.flags)};

         mylog()(3) << "    Writing header" << std::endl;

         // Write the payload header to the DBG file
         wrtr.write(dbgSrHdr);

         // Get the symbol name
         const std::string& symName = conv.getName(tdsPlHdr.nameIdx);

         // Append the length-prefixed symbol name to the payload header
         wrtr.write1bLpfName(symName);

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

         // Remember where the record is written to
         unsigned int dbgHdrPos = dbgFile.tell() - strtPos;

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);

         // Global and Local Procedure symbol records must have
         // a corresponding PUB32 record in the sstGlobalPub
         // section or a corresponding PROCREF record in the
         // sstStaticSym, respectively, of the DBG file as
         // well. We must therefore store the record for later
         // use.
         SymbolRecord sr;

         sr.typeIdx    = tdsPlHdr.typeIdx;
         sr.nameIdx    = tdsPlHdr.nameIdx;
         sr.moduleIdx  = moduleIdx;
         sr.offsSymRec = dbgHdrPos;
         sr.symRefOffs = dbgHdrPos;

         if(CVskLocalProc != recHdr.symKind || conv.staticAsPub()) {
            sr.symKind = CVskPublic;
            pubSyms[tdsPlHdr.segment][tdsPlHdr.offset] = sr;
         } else {
            sr.symKind = CVskProcRef;
            staticSyms[tdsPlHdr.segment][tdsPlHdr.offset] = sr;
         }

         // The address of the symbol should potentially be referenced
         // from an address hash table, so we need to add to the syms.
         syms[tdsPlHdr.segment][tdsPlHdr.offset] = sr;

         // The symbol name should potentially be referenced from a
         // name hash table, so we need to register it in the name hash.
         nameHshTable.append(symName,dbgHdrPos);

         break;
      }
      case CVskBlock32: {
         mylog()(3) << "   Converting S_BLOCK" << std::endl;

         // Read the payload from the TDS file
         TDSsrBlock32Payload tdsPl;
         rdr.read(tdsPl);

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of
         // payload bytes written.
         RecordWriter wrtr;

         // Create the corresponding payload header for the DBG file
         CVsrBlock32PayloadHdr dbgPlHdr =
            {tdsPl.parent,tdsPl.end,tdsPl.next,tdsPl.len,tdsPl.offset,
             tdsPl.segment};

         // Write the payload header to the DBG file
         wrtr.write(dbgPlHdr);

         // Append the length-prefixed symbol name to the payload header
         wrtr.write1bLpfName(conv.getName(tdsPl.nameIdx));

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);
         break;
      }
      case CVskSkip: {
         mylog()(3) << "   Skipping S_SKIP" << std::endl;
         // Skip this record
         break;
      }
      case CVskGProcRef: {
         // The GPROCREF's located in the sstGlobalSym of TDS files
         // doesn't contain enough info (reference to the originating
         // module is 0) for us to create a Code View PROCREF record
         // out of it. We therefore create a PUB32 in the sstGlobalPub
         // instead.

         mylog()(3) << "   Converting S_GPROCREF" << std::endl;

         /// @todo Use the segment and offset to look up the data needed
         ///       to create a Code View PROCREF as well.

         // Read the type string payload from the TDS file
         TDSsrGlobalProcRefPayload tdsPl;
         rdr.read(tdsPl);

         SymbolRecord sr;

         sr.typeIdx    = tdsPl.typeIdx;
         sr.nameIdx    = tdsPl.nameIdx;
         sr.moduleIdx  = 0xFFFF;
         sr.offsSymRec = sr.symRefOffs = dbgFile.tell()-strtPos;
         sr.symKind    = CVskPublic;
         pubSyms[tdsPl.segment][tdsPl.offset] = sr;
         break;
      }
      case CVskGDataRef: {
         // The GDATAREF's located in the sstGlobalSym of TDS files
         // doesn't contain enough info (reference to the originating
         // module is 0) for us to create a Code View PROCREF record
         // out of it. We therefore create a PUB32 in the sstGlobalPub
         // instead.

         mylog()(3) << "   Converting S_GDATAREF" << std::endl;

         /// @todo Use the segment and offset to look up the data needed
         ///       to create a Code View DATAREF as well.

         // Read the type string payload from the TDS file
         TDSsrGlobalDataRefPayload tdsPl;
         rdr.read(tdsPl);

         SymbolRecord sr;

         sr.typeIdx    = tdsPl.typeIdx;
         sr.nameIdx    = tdsPl.nameIdx;
         sr.moduleIdx  = 0xFFFF;
         sr.offsSymRec = sr.symRefOffs = dbgFile.tell()-strtPos;
         sr.symKind    = CVskPublic;
         pubSyms[tdsPl.segment][tdsPl.offset] = sr;
         break;
      }
      case CVskNamespace: // Intentional fall-through
      case CVskUsing:
         // We currently don't know how to convert this. There does
         // not seem to be a corresponding symbol kind in CodeView.
      case CVskEntry32:
         // We currently don't know how to convert this. Borlands
         // documentation states that this symbol record is optional,
         // so we leave it out.
         break;
      case CVskOptVar32:
         // Variable life range support. We don't know how to
         // convert this, currently.
         break;
      case  CVskProcRet32:
         // Procedure return epilogue.
         // Don't know how to convert this.
         break;
      case CVskSaveRegs32:
         // Don't know how to convert this.
         break;
      default:
         mylog()(1)
            << "Warning: Unhandled symbol record with symbol kind "
            << std::hex << recHdr.symKind << " at "
            << tdsHdrPos << " with length " << recLen << std::endl;
         break;
      }

      // Jump to the next symbol record
      rdr.seek(tdsHdrPos + sizeof(recHdr.len)+ recLen);
   }

   // Symbol table must end with a closing S_ALIGN
   appendClosingAlign(dbgFile);

   // Compute the size of the contructed symbol table.
   symTableSz = dbgFile.tell()-strtPos;
}

/// @brief Converts a numeric leaf from a TDS type record to a numeric leaf
///        in a DBG type record.
///
/// @param leafId  The id of the numeric leaf.
/// @param tsPos   The position within the TDS file from which the leaf Id
///                was read.
/// @param tsRdr   Type string reader.
/// @param tsWrtr  Type string writer.
///
/// @throw runtime_error In case of errors.
void convertNumericLeaf(WORD          leafId,
                        unsigned int  tsPos, 
                        RecordReader& tsRdr,
                        RecordWriter& tsWrtr) throw(std::runtime_error)
{
   switch(leafId) {
   case LF_CHAR:
      tsWrtr.write(leafId);
      copyFileContent(tsWrtr,tsRdr,1);
      break;
   case LF_SHORT: // Intentional fall-through
   case LF_USHORT:
      tsWrtr.write(leafId);
      copyFileContent(tsWrtr,tsRdr,2);
      break;
   case LF_LONG: // Intentional fall-through
   case LF_ULONG:
   case LF_REAL32:
   case LF_COMPLEX32:
      tsWrtr.write(leafId);
      copyFileContent(tsWrtr,tsRdr,4);
      break;
   case LF_REAL48:
      tsWrtr.write(leafId);
      copyFileContent(tsWrtr,tsRdr,6);
      break;            
   case LF_REAL64: // Intentional fall-through
   case LF_QUADWORD:
   case LF_UQUADWORD:
   case LF_COMPLEX64:
      tsWrtr.write(leafId);
      copyFileContent(tsWrtr,tsRdr,8);
      break;
   case LF_REAL80:
   case LF_COMPLEX80:
      tsWrtr.write(leafId);
      copyFileContent(tsWrtr,tsRdr,10);
      break;
   case LF_REAL128:
   case LF_COMPLEX128:
      tsWrtr.write(leafId);
      copyFileContent(tsWrtr,tsRdr,16);
      break;
   case LF_VARSTRING:
      unsigned short len;
      tsRdr.read(len);
      tsWrtr.write(leafId);
      tsWrtr.write(len);
      copyFileContent(tsWrtr,tsRdr,len);
      break;
   default:
      if(leafId<LF_NUMERIC) {
         // The leafId must be interpreted as the value.
         tsWrtr.write(leafId);
      } else {
         mylog()(1)
            << "Error: Unhandled type record with leaf id "
            << std::hex << leafId << " at " << tsPos << std::endl;
      }
      break;
   }
}

/// @brief Converts a numeric leaf from a TDS type record to a numeric leaf
///        in a DBG type record.
///
/// @param tsRdr   Type string reader.
/// @param tsWrtr  Type string writer.
///
/// @throw runtime_error In case of errors.
void copyNumericLeaf(RecordReader& tsRdr,
                     RecordWriter& tsWrtr) throw(std::runtime_error)
{
   unsigned int tsPos = tsRdr.tell();

   WORD         leafId;

   tsRdr.read(leafId);

   convertNumericLeaf(leafId,tsPos,tsRdr,tsWrtr);
}

/// @brief Converts a table of TDS type records to a corresponding table
///        of DBG type records.
///
/// See "Types Definition Segment" in Tools Interface Standard Formats
/// Specification for Windows version 1.0 for the definition of a DBG
/// type record and type string.
///
/// @param      conv         Converter to use for various conversion tasks.
/// @param      tdsFile      The input tds file.
/// @param      numTdsTypes  The number of types in the input tds file.
/// @param      tdsTableSz   The size of the type table in the input.
/// @param      unsigned int The position of the sstGlobalTypes.
/// @param      dbgFile      The output dbg file.
/// @param[out] numDbgTypes  The number of types in the output dbg file.
///
/// @throw runtime_error In case of errors.
///
/// @todo This part of the implementation is incomplete.
void convertTypes(const Converter& conv,
                  FileWrapper&     tdsFile,
                  DWORD            numTdsTypes,
                  DWORD            tdsTableSz,
                  unsigned int     subSecPos,
                  FileWrapper&     dbgFile,
                  DWORD&           numDbgTypes) throw(std::runtime_error)
{
   // Establish a reader for the TDS type records.
   RecordReader recRdr(tdsFile,tdsTableSz);

   // Remember where we start writing type record offsets to.
   unsigned int frstDbgTrOffsPos = dbgFile.tell();
   unsigned int dbgTrOffsPos     = frstDbgTrOffsPos;

   // Compute the position of the first type record in the DBG file.
   // For TDS files, type record offsets are relative to the start of the
   // sstGlobalTypes subsection. 
   // For DBG NB09 files, type record offsets are relative to the location of
   // the first type record.
   unsigned int frstDbgTrPos = dbgTrOffsPos + sizeof(DWORD)*numTdsTypes;

   // Position of the current type record
   unsigned int nextDbgTrPos = frstDbgTrPos;

   while(numTdsTypes && !recRdr.eor()) {

      // Read the offset of the type record in the TDS file..
      DWORD tdsTrOffs;
      recRdr.read(tdsTrOffs);

      // Remember where to read the next type record offset from.
      unsigned int nextTdsTrOffsPos = recRdr.tell();

      // Move to the position of the type record in the TDS file.
      recRdr.seek(subSecPos+tdsTrOffs);

      // Read a type record header (length field) from the TDS file
      CVtrHeader tdsRecHdr;
      recRdr.read(tdsRecHdr);

      // Establish a writer for the header (length field) of the DBG type
      // record.
      CVtrHeader                     dbgRecHdr;
      HeaderWithLen<CVtrHeader,WORD> recWrtr(dbgRecHdr,dbgRecHdr.len);

      // Establish a reader for the TDS type strings
      RecordReader                   tsRdr(tdsFile,tdsRecHdr.len);

      // Establish a writer for the DBG type strings
      RecordWriter                   tsWrtr;

      // Loop over all type strings in this type record
      bool nextTypeString = false;
      while(!nextTypeString && !tsRdr.eor()) {

         // Remember the position of the type string in the TDS file
         unsigned int  tsPos = tsRdr.tell();

         // Read the leaf identifier of the TDS type string
         TDSTypeString tdsTs;
         tsRdr.read(tdsTs.leaf);

         // Prepare the DBG type string
         CVTypeString  dbgTs;
         dbgTs.leaf = tdsTs.leaf; // Set identical for a start

         switch(tdsTs.leaf) {
         case LF_MODIFIER: {
            tsRdr.read(tdsTs.modifier);
            dbgTs.modifier.attribute = tdsTs.modifier.attribute;
            dbgTs.modifier.typeIdx   = conv.typIdx(tdsTs.modifier.typeIdx);
            tsWrtr.write(dbgTs.leaf);
            tsWrtr.write(dbgTs.modifier);
            break;
         }
         case LF_POINTER: {
            tsRdr.read(tdsTs.pointer);

            unsigned char ptrType = (tdsTs.pointer.attribute & 0x1f);
            unsigned char ptrMode = (tdsTs.pointer.attribute & 0xE0) >> 5;

            if(6 == ptrType) {
               mylog()(1)
                  << "Unhandled pointer type " << std::hex << ((int) ptrType)
                  << " at " << tsPos << std::endl;
               nextTypeString = true;
            } else {
               // Convert the fixed size part
               dbgTs.pointer.attribute = tdsTs.pointer.attribute;
               dbgTs.pointer.typeIdx   = conv.typIdx(tdsTs.pointer.typeIdx);
               tsWrtr.write(dbgTs.leaf);
               tsWrtr.write(dbgTs.pointer);

               // Convert the variant part
               if(1 < ptrMode) {
                  /// @todo for pointer to data/method member, TDS
                  /// and DBG seem to interchange the position of
                  /// the class and format fields. Verify this!
                  WORD  format;
                  DWORD classIdx;

                  tsRdr.read(format);
                  tsRdr.read(classIdx);

                  WORD dbgClassIdx = conv.typIdx(classIdx);
                  tsWrtr.write(dbgClassIdx);
                  tsWrtr.write(format);

                  // Number of bytes to copy depends on format.
                  unsigned bytesToCopy = 0;

                  switch(format) {
                  case 0:
                  case 1:
                  case 5:
                     bytesToCopy = 2;
                     break;
                  case 3:
                  case 6:
                  case 8:
                  case 11:
                     bytesToCopy = 4;
                     break;
                  case 2:
                  case 4:
                  case 9:
                     bytesToCopy = 6;
                     break;
                  case 7:
                  case 12:
                     bytesToCopy = 8;
                     break;
                  case 10:
                     bytesToCopy = 10;
                     break;
                  case 13:
                     bytesToCopy = 16;
                     break;
                  default:
                     mylog()(1)
                        << "Unhandled variant format in LF_POINTER at "
                        << std::hex << tsPos << std::endl;
                     break;
                  }
                  copyFileContent(tsWrtr,tsRdr,bytesToCopy);
               } else if(3 == ptrType || 5 == ptrType) {
                  /// @todo Test this. Not sure it's correct.
                  WORD seg;
                  tsRdr.read(seg);
                  tsWrtr.write(seg);
               } else if(8 == ptrType) {
                  /// @todo Test this. Not sure it's correct.
                  mylog()(1)
                     << "Warning at " << std::hex << tsPos
                     << ": Current conversion of LF_POINTER variant part is"
                     " dubious when pointer is based on type" << std::endl;

                  // Copy type index
                  DWORD typeIdx;
                  tsRdr.read(typeIdx);
                  unsigned short dbgTypeIdx = conv.typIdx(typeIdx);
                  tsWrtr.write(dbgTypeIdx);

                  // Copy length prefixed name
                  BYTE l;
                  tsRdr.read(l);
                  tsWrtr.write(l);
                  copyFileContent(tsWrtr,tsRdr,l);
               }
            }
            break;
         }
         case LF_ARRAY: {
            tsRdr.read(tdsTs.array);
            dbgTs.array.elemTypeIdx = conv.typIdx(tdsTs.array.elemTypeIdx);
            dbgTs.array.idxTypeIdx  = conv.typIdx(tdsTs.array.idxTypeIdx);
            tsWrtr.write(dbgTs.leaf);
            tsWrtr.write(dbgTs.array);

            // Convert the numerical leaf representing the length of the array
            unsigned int numLeafPos = tsRdr.tell();
            WORD         numLeafId;

            tsRdr.read(numLeafId);

            convertNumericLeaf(numLeafId,numLeafPos,tsRdr,tsWrtr);

            tsWrtr.write1bLpfName(conv.getName(tdsTs.array.nameIdx));
            break;
         }
         case LF_CLASS: // Intentional fall-through
         case LF_STRUCTURE: {
            tsRdr.read(tdsTs.cls);
            dbgTs.cls.count        = tdsTs.cls.count;
            dbgTs.cls.fieldTypeIdx = conv.typIdx(tdsTs.cls.fieldTypeIdx);
            dbgTs.cls.property = tdsTs.cls.property;
            // Set bit 8 of the property field according to whether or
            // not the declaration of this class or structure is
            // contained within another.
            if(tdsTs.cls.cClass) {
               dbgTs.cls.property |= 0x80;
            } else {
               dbgTs.cls.property &= 0xff7f;
            }
            dbgTs.cls.dList  = conv.typIdx(tdsTs.cls.dList);
            dbgTs.cls.vShape = conv.typIdx(tdsTs.cls.vShape);
            tsWrtr.write(dbgTs.leaf);
            tsWrtr.write(dbgTs.procedure);
            copyNumericLeaf(tsRdr,tsWrtr);
            tsWrtr.write1bLpfName(conv.getName(tdsTs.cls.nameIdx));
            break;
         }
         case LF_PROCEDURE: {
            tsRdr.read(tdsTs.procedure);
            dbgTs.procedure.rvTypeIdx =
               conv.typIdx(tdsTs.procedure.rvTypeIdx);
            dbgTs.procedure.callConv = tdsTs.procedure.callConv;
            dbgTs.procedure.reserved = 0;
            dbgTs.procedure.parms    = tdsTs.procedure.parms;
            dbgTs.procedure.argList  = tdsTs.procedure.argList;
            tsWrtr.write(dbgTs.leaf);
            tsWrtr.write(dbgTs.procedure);
            break;
         }
         case LF_MFUNCTION: {
            tsRdr.read(tdsTs.mFunction);
            dbgTs.mFunction.rvTypeIdx =
               conv.typIdx(tdsTs.mFunction.rvTypeIdx);
            dbgTs.mFunction.classIdx =
               conv.typIdx(tdsTs.mFunction.classIdx);
            dbgTs.mFunction.thisIdx =
               conv.typIdx(tdsTs.mFunction.thisIdx);
            dbgTs.mFunction.callConv = tdsTs.mFunction.callConv;
            dbgTs.mFunction.reserved = 0;
            dbgTs.mFunction.parms = tdsTs.mFunction.parms;
            dbgTs.mFunction.argList =
               conv.typIdx(tdsTs.mFunction.argList);
            dbgTs.mFunction.thisAdjust =
               tdsTs.mFunction.thisAdjust;
            break;
         }
         case LF_NOTTRANS: {
            tsWrtr.write(dbgTs.leaf);
            break;
         }
         case LF_PAD0: // Intentional fall-throughs
         case LF_PAD1:
         case LF_PAD2:
         case LF_PAD3:
         case LF_PAD4:
         case LF_PAD5:
         case LF_PAD6:
         case LF_PAD7:
         case LF_PAD8:
         case LF_PAD9:
         case LF_PAD10:
         case LF_PAD11:
         case LF_PAD12:
         case LF_PAD13:
         case LF_PAD14:
         case LF_PAD15:
            // Skip all these --- we'll do our own alignment.
            break;
         case LF_ARGLIST: {
            // Copy the number of arguments in the list.
            WORD argCount;
            tsRdr.read(argCount);
            tsWrtr.write(argCount);

            // In TDS files: argCount 4-byte type indices.
            // In DBG files: argCount 2-byte type indices.
            while(!tsRdr.eor() && argCount) {
               DWORD typeIdx;
               tsRdr.read(typeIdx);
               WORD dbgTypeIdx = conv.typIdx(typeIdx);
               tsWrtr.write(dbgTypeIdx);
               --argCount;
            }
            break;
         }  
         case LF_MLIST: {
            tsWrtr.write(dbgTs.leaf);
            TDStsMListElem tdsElem;
            CVtsMListElem  dbgElem;
            while(!tsRdr.eor()) {
               tsRdr.read(tdsElem);
               dbgElem.attribute = tdsElem.attribute;
               dbgElem.typeIdx   = conv.typIdx(tdsElem.typeIdx);
               tsWrtr.write(dbgElem);
               unsigned int mProp = dbgElem.attribute & MEMBATTR_PROP_MASK;
               // The 4-byte vtaboffset is only present when the method
               // property is introducing virtual.
               if(MEMBATTR_PROP_INTVIRT == mProp ||
                  MEMBATTR_PROP_INTPURE == mProp) {
                  unsigned int vTabOffs;
                  tsRdr.read(vTabOffs);
                  tsWrtr.write(vTabOffs);
               }
            }
            break;
         }
         case LF_CHAR:
         case LF_SHORT: // Intentional fall-through
         case LF_USHORT:
         case LF_LONG: // Intentional fall-through
         case LF_ULONG:
         case LF_REAL32:
         case LF_COMPLEX32:
         case LF_REAL48:
         case LF_REAL64: // Intentional fall-through
         case LF_QUADWORD:
         case LF_UQUADWORD:
         case LF_COMPLEX64:
         case LF_REAL80:
         case LF_COMPLEX80:
         case LF_REAL128:
         case LF_COMPLEX128:
         case LF_VARSTRING:
            convertNumericLeaf(tdsTs.leaf,tsPos,tsRdr,tsWrtr);
            break;
         default:
            // Set all unhandled types as NOTTRANS
            dbgTs.leaf = LF_NOTTRANS;
            tsWrtr.write(dbgTs.leaf);

            // Skip to the next type record as we cannot safely
            // continue with this type record.
            nextTypeString = true;

            mylog()(1)
               << "Error: Unhandled type record with leaf id "
               << std::hex << tdsTs.leaf << " at " << tsPos << std::endl;
         }

         // Ensure that the next type string is properly aligned on a
         // dword boundary
         unsigned int bytesToWrite = recWrtr.length(tsWrtr);
         unsigned int malgnmnt = bytesToWrite & 0x3;
         if(malgnmnt) {
            unsigned char padBytes  = 3 - (unsigned char) malgnmnt;
            // Determine which LF_PADxx identifier to write out.
            unsigned char alignLeaf = 0xEF + padBytes;
            tsWrtr.write(alignLeaf);
            // Append padding bytes.
            alignLeaf = 0;
            tsWrtr.write(alignLeaf,padBytes);
         }
      }

      // Move to where the type record is to be written
      dbgFile.seek(nextDbgTrPos);

      // Ensure that the record does not cross a page boundary.
      pageAlign(dbgFile,recWrtr.length(tsWrtr),0xBFFF);

      // Remember the write position after alignment relative to the
      // first type record.
      unsigned int dbgTrPos = (dbgFile.tell() - frstDbgTrPos);

      // Update the record header length field and output the header and
      // the data registered with the record writer.
      recWrtr.commit(dbgFile,tsWrtr);
      ++numDbgTypes;

      // Remember where to write the next type record to.
      nextDbgTrPos = dbgFile.tell();

      // Move to the position of the type record offset in the DBG file.
      dbgFile.seek(dbgTrOffsPos);

      // Write the offset of the type record to the DBG file.
      dbgFile.write(dbgTrPos);

      // Remember where to write the next type recor offset to the DBG file.
      dbgTrOffsPos = dbgFile.tell();

      // Move to read the next type record offset
      recRdr.seek(nextTdsTrOffsPos);
      --numTdsTypes;
   }
}

/// @brief Creates the sstGlobalPub subsection.
///
/// @param      conv           Converter to use for various conversion tasks.
/// @param      dbgFile        The DBG file to write to.
/// @param[out] symTablSz      Symbol table size.
/// @param[out] symHshTableSz  Symbol hash table size.
/// @param[out] addrHshTableSz Address hash table size.
/// @param      syms           The public symbols.
///
/// @throw runtime_error In case of errors.
void createGlobalPub(const Converter& conv,
                     FileWrapper&     dbgFile,
                     DWORD&           symTableSz,
                     DWORD&           symHshTableSz,
                     DWORD&           addrHshTableSz,
                     SymRecMap&       syms) throw(std::runtime_error)
{
   // Create the internal name hash table
   NameHshTbl nameHshTbl(NUM_HASH_BUCKETS);

   // Remember the DBG file offset of the first symbol
   unsigned int frstSymOffs = dbgFile.tell();

   // All symbol records in sstGlobalPub have kind PUB32.
   CVsymRecHdr recHdr;
   recHdr.symKind = CVskPublic;

   SymRecMap::iterator segIt = syms.begin();

   // Loop over all segments
   while(syms.end() != segIt) {

      SegmSymMap::iterator symIt = segIt->second.begin();

      // Loop over all symbols within that segment
      while(segIt->second.end() != symIt) {

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of
         // payload bytes written.
         RecordWriter wrtr;

         // Create the corresponding payload header for the DBG file
         CVsrGlobalDataPayloadHdr dbgPlHdr =
            {symIt->first,
             segIt->first,
             conv.typIdx(symIt->second.typeIdx)};

         // Write the payload header to the DBG file
         wrtr.write(dbgPlHdr);

         // Look up the symbol name
         const std::string& symName = conv.getName(symIt->second.nameIdx);

         // Append the length-prefixed symbol name to the payload header
         wrtr.write1bLpfName(symName);

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),0xFFF);

         // Store the relative offset of the current symbol record.
         // We will need this later on for the address hash table.
         // Note that this field may have been set elsewhere (e.g.
         // in convertSymbolSubsection) to point to the position
         // where the original symbol was found. We don't need that
         // here, however.
         symIt->second.symRefOffs = dbgFile.tell() - frstSymOffs;

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);
               
         // Update the internal name hash table
         nameHshTbl.append(symName,symIt->second.symRefOffs);

         // Next symbol
         ++symIt;
      }

      // Next segment
      ++segIt;
   }

   // Symbol table must end with a closing S_ALIGN
   appendClosingAlign(dbgFile);

   // Compute the size of the symbol table
   symTableSz = dbgFile.tell()-frstSymOffs;

   // Proceed to write the symbol hash table
   nameHshTbl.write(dbgFile,symHshTableSz);

   // Proceed to write the address hash table
   createAddrHashTable(dbgFile,addrHshTableSz,syms);
}

/// @brief Creates the sstStaticSym or similar subsection.
///
/// @param      conv           Converter to use for various conversion tasks.
/// @param      dbgFile        The DBG file to write to.
/// @param[out] symTableSz     Symbol table size.
/// @param[out] symHshTableSz  Symbol hash table size.
/// @param[out] addrHshTableSz Address hash table size.
/// @param      syms           The symbols to write to the subsection.
/// @param      algnmnt        The size of pages. No symbol must cross a page.
///                            0 if none.
///
/// @throw runtime_error In case of errors.
void createSymbolsSubsection(const Converter& conv,
                             FileWrapper&     dbgFile,
                             DWORD&           symTableSz,
                             DWORD&           symHshTableSz,
                             DWORD&           addrHshTableSz,
                             SymRecMap&       syms,
                             unsigned int     algnmnt) throw(std::runtime_error)
{
   // Create the internal name hash table
   NameHshTbl nameHshTbl(NUM_HASH_BUCKETS);

   // Remember where we start writing to
   unsigned int strtPos = dbgFile.tell();

   CVsymRecHdr recHdr;

   SymRecMap::iterator segIt = syms.begin();

   // Loop over all segments
   while(syms.end() != segIt) {

      // Loop over all symbols within this segment
      SegmSymMap::iterator symIt = segIt->second.begin();

      while(segIt->second.end() != symIt) {

         // Set the symbol kind in the record header
         recHdr.symKind = symIt->second.symKind;

         // Reserve space for a DBG symbol record header to be
         // written out later.
         HeaderWithLen<CVsymRecHdr,WORD> dbgRecHdr(recHdr,recHdr.len);

         // Create a record writer to keep track of the number of
         // payload bytes written.
         RecordWriter wrtr;

         // Create the corresponding payload header for the DBG file
         CVsrDataRefPayload dbgPl =
            {0,symIt->second.offsSymRec,symIt->second.moduleIdx};

         // Write the payload header to the DBG file
         wrtr.write(dbgPl);

         // Ensure that the next record is properly aligned on a
         // dword boundary
         zeroPad(wrtr,dbgRecHdr.length(wrtr),sizeof(int)-1);

         // Ensure that the record does not cross a page boundary.
         pageAlignSymbol(dbgFile,dbgRecHdr.length(wrtr),algnmnt);

         // Store the relative offset of the current symbol record.
         // We will need this later on for the address hash table.
         // Note that this field may have been set elsewhere (e.g.
         // in convertSymbolSubsection) to point to the position
         // where the original symbol was found. We don't need that
         // here, however.
         symIt->second.symRefOffs = dbgFile.tell() - strtPos;

         // Write the symbol record header with the correct length field
         dbgRecHdr.commit(dbgFile,wrtr);
               
         // Look up the symbol name
         const std::string& symName = conv.getName(symIt->second.nameIdx);

         // Update the internal name hash table
         nameHshTbl.append(symName,symIt->second.symRefOffs);

         // Next symbol
         ++symIt;
      }

      // Next segment
      ++segIt;
   }

   // Some symbol tables must end with a closing S_ALIGN - just do it for all.
   appendClosingAlign(dbgFile);

   // Compute the size of the generated symbol table
   symTableSz = dbgFile.tell() - strtPos;

   // Proceed to write the symbol hash table
   nameHshTbl.write(dbgFile,symHshTableSz);

   // Proceed to write the address hash table
   createAddrHashTable(dbgFile,addrHshTableSz,syms);
}

/// @brief TDS to Code View conversion algorithm
///
/// @param conv     The converter context to use.
/// @param tdsCvHdr The header already read from the TDS file.
/// @param tdsFile  The TDS file to read from. The current read position
///                 is assumed to be right after the CVHdr already read in.
/// @param segMap   Input for the sstSegMap subsection.
/// @param dbgFile  The DBG file to read from.
/// @param lfaBase  Location of the Code View header in the output file.
///
/// @throw runtime_error In case of errors.
void convertTdsToCodeView(Converter&                            conv,
                          CVHdr&                                tdsCvHdr,
                          FileWrapper&                          tdsFile,
                          const std::vector<CVssSegMapSegDesc>& segMap,
                          FileWrapper&                          dbgFile,
                          unsigned int                          lfaBase)
   throw(std::runtime_error)
{
   // Move to the location of the Subsection directory in the tds file.
   tdsFile.seek(tdsCvHdr.lfoDir);

   // Read the Subsection directory header from the TDS file.
   CVDirHdr tdsCvDirHdr;
   tdsFile.read(tdsCvDirHdr);

   // Read select subsection directory entries from the TDS files
   typedef std::vector<CVDirEntry> CVDirEntryList;
   CVDirEntryList tdsSubSecDirEntries;

   mylog()(2) << "TDS subsection directory header has "
              << tdsCvDirHdr.cDir << " entries." << std::endl;

   for(DWORD i=0 ; i<tdsCvDirHdr.cDir ; ++i) {
      CVDirEntry dirEntry;
      tdsFile.seek(tdsCvHdr.lfoDir+sizeof(CVDirHdr)+i*(sizeof dirEntry));
      tdsFile.read(dirEntry);
      // TDS files appear to be using module index 0 in places where
      // DBG files are apparently using 0xFFFF.
      if(0 == dirEntry.iMod) {
         dirEntry.iMod = 0xFFFF;
      }
      switch(dirEntry.subsection) {
      case CVsstIdxModule:
         tdsSubSecDirEntries.push_back(dirEntry);
         break;
      case CVsstIdxSrcModule:
         tdsSubSecDirEntries.push_back(dirEntry);
         break;
      case CVsstIdxNames:
         conv.loadNameCache(tdsFile,dirEntry.lfo,dirEntry.cb);
         break;
      case CVsstIdxAlignSym:
         tdsSubSecDirEntries.push_back(dirEntry);
         break;
      case CVsstIdxGlobalSym:
         tdsSubSecDirEntries.push_back(dirEntry);
         break;
      case CVsstIdxGlobalTypes:
         tdsSubSecDirEntries.push_back(dirEntry);
         break;
      default:
         mylog()(1)
            << "Warning: Unhandled subsection with tag "
            << std::hex << dirEntry.subsection << std::endl;
         break;
      }
   }

   // A place holder for the contents of the sstGlobalPub to be generated
   // on the fly.
   SymRecMap sstGlobalPubInput;

   // A place holder for the contents of the sstStaticSym to be generated
   // on the fly.
   SymRecMap sstStaticSymInput;

   // Module index counter
   unsigned short moduleIdx = 0;

   // Iterate over the subsections in the TDS file and move them
   // to the DBG file.
   CVDirEntryList                 dbgSubSecDirEntries;
   CVDirEntryList::const_iterator subSecIt = tdsSubSecDirEntries.begin();

   while(tdsSubSecDirEntries.end() != subSecIt) {
      CVDirEntry dirEntry = *subSecIt;

      // Move to the location of the subsection in the TDS file
      tdsFile.seek(dirEntry.lfo);

      // Adjust the location of the subsection to fall on a DWORD boundary
      unsigned int misalignment = dbgFile.tell() & 0x3;
      if(misalignment) {
         dbgFile.skip(4-misalignment);
      }

      // Remember where we start writing to. This is often used as the
      // base of all file offsets in the subsection.
      long int dbgSubSecBase = dbgFile.tell();

      // Set the location of the subsection in the DBG File
      dirEntry.lfo = dbgSubSecBase-lfaBase;

      switch(dirEntry.subsection) {
      case CVsstIdxModule: {
         mylog()(2) << "Converting sstModule found at "
                    << std::hex << tdsFile.tell()
                    << ". Writing at " << dbgFile.tell()
                    << " (" << (dbgFile.tell()-lfaBase) << ")" << std::endl;

         // Get the TDSsstModuleHdr from the TDS file
         TDSssModuleHdr tdsHdr;
         tdsFile.read(tdsHdr);

         // Write the CVsstModuleHdr
         dbgFile.write(*((CVssModuleHdr*)&tdsHdr));

         // Copy the segment infos from the TDS file to the DBG file
         CVssModuleSegInfo segInfo;
         for(WORD i=0 ; i<tdsHdr.cSeg ; ++i) {
            tdsFile.read(segInfo);
            dbgFile.write(segInfo);
         }

         // Write the rest of the CVssModule
         dbgFile.write1bLpfName(conv.getName(tdsHdr.nameIdx));

         // Compute the size of the subsection
         dirEntry.cb = dbgFile.tell() - dbgSubSecBase;

         // Update the directory
         dbgSubSecDirEntries.push_back(dirEntry);
         break;
      }

      case CVsstIdxSrcModule: {
         mylog()(2) << "Converting sstSrcModule found at "
                    << std::hex << tdsFile.tell()
                    << ". Writing at " << dbgFile.tell()
                    << " (" << (dbgFile.tell()-lfaBase) << ")" << std::endl;

         // Copy the header for the fixed part of TDSssSrcModule::hdr
         // from the TDS file to the DBG file.
         CVssSrcModuleFixedHdr fixedHdr;
         tdsFile.read(fixedHdr);
         dbgFile.write(fixedHdr);

         // Read in TDSssSrcModule::hdr.baseSrcFile from the TDS file.
         std::vector<DWORD> tdsBaseSrcFile(fixedHdr.cFile);
         tdsFile.read(tdsBaseSrcFile[0],tdsBaseSrcFile.size());

         // Skip writing CVssSrcModule::hdr.baseSrcFile to the DBG
         // file for now. We will compute and write it later on.
         long int fileOffsOfbaseSrcFile = dbgFile.tell();
         dbgFile.skip(sizeof(DWORD)*fixedHdr.cFile);

         // Prepare the vector that will contain the data for
         // CVssSrcModule::hdr.baseSrcFile
         std::vector<DWORD> dbgBaseSrcFile;

         // Copy the TDSssSrcModule::hdr.startEnd to
         // CVssSrcModule::hdr.startEnd
         copyFileContent(dbgFile,tdsFile,2*sizeof(DWORD)*fixedHdr.cSeg);

         // Copy the TDSssSrcModule::hdr.seg to CVssSrcModule::hdr.seg
         copyFileContent(dbgFile,tdsFile,sizeof(WORD)*fixedHdr.cSeg);

         // Copy the TDSssSrcModule::files to CVssSrcModule::files
         std::vector<DWORD>::const_iterator srcFileIt = tdsBaseSrcFile.begin();
         while(tdsBaseSrcFile.end() != srcFileIt) {
            dbgBaseSrcFile.push_back(dbgFile.tell()-dbgSubSecBase);

            TDSssSrcModuleFileHdr tdsFileHdr;
            tdsFile.seek(subSecIt->lfo+*srcFileIt);
            tdsFile.read(tdsFileHdr);

            CVssSrcModuleFileHdr dbgFileHdr = { tdsFileHdr.cSeg, 0 };
            dbgFile.write(dbgFileHdr);

            // Read in TDSssSrcModuleFile::baseSrcLn from the TDS file.
            std::vector<DWORD> tdsBaseSrcLn(dbgFileHdr.cSeg);
            tdsFile.read(tdsBaseSrcLn[0],tdsBaseSrcLn.size());

            // Skip writing CVssSrcModuleFile::baseSrcLn to the DBG
            // file as we will write it later on when the offsets are known.
            long int fileOffsOfbaseSrcLen = dbgFile.tell();
            dbgFile.skip(sizeof(DWORD)*tdsFileHdr.cSeg);

            // Prepare a vector for the contents of baseSrcLn
            std::vector<DWORD> dbgBaseSrcLen;
            
            // Copy TDSssSrcModuleFile::startEnd to CVssSrcModuleFile::startEnd
            copyFileContent(dbgFile,tdsFile,2*sizeof(DWORD)*tdsFileHdr.cSeg);

            // Write out CVssSrcModuleFile::nameLen and CVsrcModuleFile::name
            dbgFile.write1bLpfName(conv.getName(tdsFileHdr.nameIdx));

            // Copy the CVssSrcModuleSeg's
            std::vector<DWORD>::const_iterator segIt = tdsBaseSrcLn.begin();
            while(tdsBaseSrcLn.end() != segIt) {
               dbgBaseSrcLen.push_back(dbgFile.tell()-dbgSubSecBase);

               // Copy CVssSrcModuleSeg::hdr
               CVssSrcModuleSegHdr segHdr;
               tdsFile.seek(subSecIt->lfo+*segIt);
               tdsFile.read(segHdr);
               dbgFile.write(segHdr);

               // Copy CVssSrcModuleSeg::offset and
               // CVssSrcModuleSeg::linenumber
               copyFileContent(dbgFile,tdsFile,(sizeof(WORD)+sizeof(DWORD))*segHdr.cPair);

               ++segIt;
            }

            // Remember where we are
            long int moduleRecEnd = dbgFile.tell();

            // Write the correct offsets into CVssSrcModuleFile::baseSrcLen
            dbgFile.seek(fileOffsOfbaseSrcLen);
            dbgFile.write(dbgBaseSrcLen[0],dbgBaseSrcLen.size());

            // Move back to where we were
            dbgFile.seek(moduleRecEnd);

            ++srcFileIt;
         }

         // Remember where we are
         long int moduleEnd = dbgFile.tell();

         // Write the correct offsets into baseSourceFile[cFile]
         dbgFile.seek(fileOffsOfbaseSrcFile);
         dbgFile.write(dbgBaseSrcFile[0],dbgBaseSrcFile.size());

         // Move back to where we were
         dbgFile.seek(moduleEnd);

         dirEntry.cb = dbgFile.tell() - dbgSubSecBase;
         dbgSubSecDirEntries.push_back(dirEntry);
         break;
      }

      case CVsstIdxGlobalSym: {
         mylog()(2) << "Converting sstGlobalSym found at "
                    << std::hex << tdsFile.tell()
                    << ". Writing at " << dbgFile.tell()
                    << " (" << (dbgFile.tell()-lfaBase) << ")" << std::endl;

         // Read in the header of the subsection
         TDSssGlobalSymHdr tdsSymHdr;
         tdsFile.read(tdsSymHdr);

         // Prepare the corresponding DBG header
         CVssGlobalSymHdr dbgSymHdr;
         dbgSymHdr.symhash  = 0x0A; // Symbol hash table provided
         dbgSymHdr.addrhash = 0x0C; // Address hash table provided
         PpRecordWriter<CVssGlobalSymHdr> hdrWrtr(dbgFile,dbgSymHdr);

         unsigned int tdsTableSz = dirEntry.cb - (sizeof tdsSymHdr);

         // Set up output argument to contain converted symbols
         SymRecMap syms;

         // Set up the internal name hash table
         NameHshTbl nameHshTbl(NUM_HASH_BUCKETS);

         // Convert the symbols
         convertSymbolSubsection(conv,0,tdsFile,tdsTableSz,
                                 dbgFile,dbgSymHdr.cbSymbol,syms,nameHshTbl,
                                 sstGlobalPubInput,sstStaticSymInput);

         // Proceed to write the symbol hash table
         nameHshTbl.write(dbgFile,dbgSymHdr.cbSymHash);

         // Proceed to write the address hash table
         createAddrHashTable(dbgFile,dbgSymHdr.cbAddrHash,syms);

         // Write the header with the update fields
         hdrWrtr.commit();
         dirEntry.cb = dbgSymHdr.cbSymbol + dbgSymHdr.cbSymHash
            + dbgSymHdr.cbAddrHash + hdrWrtr.length();

         // Postpone appending the directory entry as we need to
         // ensure that the directory entry for the sstGlobalPub
         // preceeds the directory entry for the sstGlobalSym.

         // Align next subsection on dword boundary
         zeroPad(dbgFile,dbgFile.tell(),sizeof(int)-1);

         // Proceed to generate the sstGlobalPub
         mylog()(2) << "Generating sstGlobalPub at "
                    << std::hex << dbgFile.tell()
                    << " (" << (dbgFile.tell()-lfaBase) << ")" << std::endl;

         CVDirEntry glPubDirEntry;
         glPubDirEntry.subsection = CVsstIdxGlobalPub;
         glPubDirEntry.iMod       = 0xFFFF;
         glPubDirEntry.lfo        = dbgFile.tell()-lfaBase;

         // Prepare the DBG header for this subsection
         CVssGlobalSymHdr glPubHdr;
         glPubHdr.symhash    = 0x0A; // Symbol hash table provided
         glPubHdr.addrhash   = 0x0C; // Address hash table provided

         PpRecordWriter<CVssGlobalSymHdr> hdrWrtr2(dbgFile,glPubHdr);
         
         createGlobalPub(conv,dbgFile,glPubHdr.cbSymbol,
                         glPubHdr.cbSymHash,glPubHdr.cbAddrHash,
                         sstGlobalPubInput);

         // Write the header with the update fields
         hdrWrtr2.commit();
         glPubDirEntry.cb = hdrWrtr2.length() +
            glPubHdr.cbSymbol + glPubHdr.cbSymHash + glPubHdr.cbAddrHash;

         // Update the directory
         dbgSubSecDirEntries.push_back(glPubDirEntry);
         dbgSubSecDirEntries.push_back(dirEntry);
         break;
      }

      case CVsstIdxGlobalTypes: {

         // Skip generation of this section if not requested.
         if(!conv.doConvertTypes()) {
            mylog()(2) << "Skipping contents of";
         } else {
            mylog()(2) << "Converting";
         }
         mylog()(2) << " sstGlobalTypes found at "
                    << std::hex << tdsFile.tell()
                    << ". Writing at " << dbgFile.tell()
                    << " (" << (dbgFile.tell() - lfaBase) << ")"
                    << std::endl;

         // Remember the location of the subsection
         unsigned int ssPos = tdsFile.tell(); (void) ssPos;

         CVssGlobalTypesHdr tdsTypHdr;
         tdsFile.read(tdsTypHdr);

         // Prepare the corresponding DBG header
         CVssGlobalTypesHdr dbgTypHdr = { 0x1000000, 0 };
         PpRecordWriter<CVssGlobalTypesHdr> hdrWrtr(dbgFile,dbgTypHdr);

         unsigned int tdsTableSz = dirEntry.cb - (sizeof tdsTypHdr);
         (void) tdsTableSz;

         if(conv.doConvertTypes()) {
            // Convert type table and increment dirEntry.cb correspondingly
            convertTypes(conv,tdsFile,tdsTypHdr.cTypes,tdsTableSz,ssPos,
                         dbgFile,dbgTypHdr.cTypes);
         }

         // Write the header with the updated fields
         hdrWrtr.commit();

         // Compute the size of the subsection
         dirEntry.cb = dbgFile.tell() - dbgSubSecBase;

         // Update the directory
         dbgSubSecDirEntries.push_back(dirEntry);
         break;
      }

      case CVsstIdxAlignSym: {
         mylog()(2) << "Converting sstAlignSym found at "
                    << std::hex << tdsFile.tell()
                    << ". Writing at " << dbgFile.tell()
                    << " (" << (dbgFile.tell()-lfaBase) << ")" << std::endl;

         CVssAlignSymHdr subSecHdr;
         
         tdsFile.read(subSecHdr);

         if(subSecHdr.signature != 1) {
            mylog()(1)
               << "Warning: Unidentified sstAlignSym signature!"
               << std::endl;
         }

         // Write the corresponding DBG header
         dbgFile.write(subSecHdr);

         // Convert the symbols
         DWORD        symTableSz = 0;
         unsigned int tdsTableSz = dirEntry.cb - (sizeof subSecHdr);

         // Set up a dummy internal name hash table
         NameHshTbl nameHshTbl(NUM_HASH_BUCKETS);

         // Set up output argument to contain converted symbols
         SymRecMap syms;

         // Convert the symbols
         convertSymbolSubsection(conv,++moduleIdx,
                                 tdsFile,tdsTableSz,
                                 dbgFile,
                                 symTableSz,
                                 syms,
                                 nameHshTbl,
                                 sstGlobalPubInput,
                                 sstStaticSymInput);

         // Compute the size of the subsection
         dirEntry.cb = dbgFile.tell() - dbgSubSecBase;

         // Update the directory
         dbgSubSecDirEntries.push_back(dirEntry);
         break;
      }

      default:
         mylog()(1) << "Unidentified subsection found at "
                    << std::hex << tdsFile.tell()  << std::endl;
         break;
      }
      ++subSecIt;
   }

   {
      // Proceed to generate the sstStaticSym

      // Align subsection on dword boundary
      zeroPad(dbgFile,dbgFile.tell(),sizeof(int)-1);

      mylog()(2) << "Generating sstStaticSym at "
                 << std::hex << dbgFile.tell()
                 << " (" << (dbgFile.tell()-lfaBase) << ")" << std::endl;

      CVDirEntry dirEntry;
      dirEntry.subsection = CVsstIdxStaticSym;
      dirEntry.iMod       = 0xFFFF;
      dirEntry.lfo        = dbgFile.tell()-lfaBase;

      // Prepare the DBG header for this subsection
      CVssGlobalSymHdr dbgHdr;
      dbgHdr.symhash    = 0x0A; // Symbol hash table provided
      dbgHdr.addrhash   = 0x0C; // Address hash table provided
      PpRecordWriter<CVssGlobalSymHdr> hdrWrtr(dbgFile,dbgHdr);
         
      createSymbolsSubsection(conv,dbgFile,dbgHdr.cbSymbol,dbgHdr.cbSymHash,
                              dbgHdr.cbAddrHash,sstStaticSymInput,0);

      // Write the header with the update fields
      hdrWrtr.commit();

      // Compute the size of the subsection
      dirEntry.cb = dbgHdr.cbSymbol + dbgHdr.cbSymHash + dbgHdr.cbAddrHash
         + hdrWrtr.length();
            
      // Push a matching entry to the tail of the directory
      dbgSubSecDirEntries.push_back(dirEntry);
   }

   // TDS files do not contain a sstSegMap. Make sure one is created in the
   // DBG file.
   {
      // Align subsection on dword boundary
      zeroPad(dbgFile,dbgFile.tell(),sizeof(int)-1);

      mylog()(2) << "Generating sstSegMap at "
                 << std::hex << dbgFile.tell()
                 << " (" << (dbgFile.tell()-lfaBase) << ")" << std::endl;

      CVDirEntry dirEntry;
      dirEntry.subsection = CVsstIdxSegMap;
      dirEntry.iMod       = 0xFFFF;
      dirEntry.lfo        = dbgFile.tell()-lfaBase;

      RecordWriter wrtr;

      CVssSegMapHdr segMapHdr = {(WORD) segMap.size(), (WORD) segMap.size()};
      wrtr.write(segMapHdr);

      std::vector<CVssSegMapSegDesc>::const_iterator segIt = segMap.begin();
      while(segMap.end() != segIt) {
         wrtr.write(*segIt);
         ++segIt;
      }

      // Write the contents
      wrtr.commit(dbgFile);

      // Compute the size of the subsection
      dirEntry.cb = wrtr.length();

      // Push a matching entry to the tail of the directory
      dbgSubSecDirEntries.push_back(dirEntry);
   }

   // Prepare the CodeView header.
   CVHdr dbgCvHdr = {{'N','B','0','9'},sizeof(CVHdr)}; // Due to bounds checking with zero terminator this must be assigned as array of 4 chars rather than string

   // Store the offset (relative to lfaBase) of the subsection
   // directory header in the CodeView header.
   dbgCvHdr.lfoDir = dbgFile.tell() - lfaBase;

   // Create the data for the subsection directory header in the DBG file.
   CVDirHdr dbgCvDirHdr = tdsCvDirHdr;
   dbgCvDirHdr.cDir = dbgSubSecDirEntries.size();

   // Write the subsection directory header to the DBG file.
   dbgFile.write(dbgCvDirHdr);

   // Write out all subsection directory headers for the DBG file.
   for(DWORD i = 0; i<dbgSubSecDirEntries.size() ; ++i) {
      dbgFile.write(dbgSubSecDirEntries[i]);
   }

   // Prepare tail signature
   CVHdr dbgCvTail = {{'N','B','0','9'},sizeof(CVHdr)}; // Due to bounds checking with zero terminator this must be assigned as array of 4 chars rather than string

   // Last 4 bytes (dbgCvTail.lfoDir) must contain
   // lfoBase = length_of_file - lfaBase
   dbgCvTail.lfoDir = dbgFile.tell() + sizeof(CVHdr) - lfaBase;

   // Write out the trailing NB09 signature
   mylog()(2) << "Writing tail signature at " << std::hex
              << dbgFile.tell() << std::endl;
   dbgFile.write(dbgCvTail);

   // Move to the position of the to-be-written CodeView header
   dbgFile.seek(lfaBase);
   mylog()(2) << "Writing head signature at " << std::hex
              << dbgFile.tell() << std::endl;

   // Write out the CodeView header
   dbgFile.write(dbgCvHdr);
}

/// @brief TDS to DBG conversion algorithm
///
/// @param ef               EXE file.
/// @param tf               TDS file.
/// @param df               DBG file.
/// @param doConvTypes      Should types be converted?
/// @param staticsAsPublics Treat statics as publics?
/// @param bareCodeView     Write bare code view (no headers)?
///
/// @todo doConvTypes == true is incomplete.
///
/// @throw runtime_error In case of errors.
void genDbgFromExeNTds(FILE*        ef,
                       FILE*        tf,
                       FILE*        df,
                       bool         doConvTypes,
                       bool         staticsAsPublics,
                       bool         bareCodeView) throw(std::runtime_error)
{
   // Create wrappers for the TDS and DBG files.
   FileWrapper tdsFile(tf);
   FileWrapper dbgFile(df);

   // Converter to use for various conversion tasks
   Converter conv(doConvTypes,staticsAsPublics);
   
   // Read the DOS header of the executable PE file.
   IMAGE_DOS_HEADER dosHdr;
   if(1 != fread(&dosHdr,(sizeof dosHdr),1,ef)) {
      throw std::runtime_error("Could not read DOS_HEADER from exe file");
   }

   // Move to the NT headers of the executable
   fseek(ef,dosHdr.e_lfanew,SEEK_SET);

   // Read the NT headers of the executable
   IMAGE_NT_HEADERS ntHdrs;
   if(1 != fread(&ntHdrs,(sizeof ntHdrs),1,ef)) {
      throw std::runtime_error("Could not read NT_HEADERS from exe file");
   }

   // Mark the exe file as stripped for debug info
   if(0 ==(ntHdrs.FileHeader.Characteristics & IMAGE_FILE_DEBUG_STRIPPED)) {
      ntHdrs.FileHeader.Characteristics |= IMAGE_FILE_DEBUG_STRIPPED;
      fseek(ef,dosHdr.e_lfanew,SEEK_SET);
      fwrite(&ntHdrs,(sizeof ntHdrs),1,ef);
      fseek(ef,dosHdr.e_lfanew+(sizeof ntHdrs),SEEK_SET);
   }

   // Bare code view has no PE header
   if(!bareCodeView) {

      // Prepare the header of the DBG PE file.
      IMAGE_SEPARATE_DEBUG_HEADER dbgHdr;
      memset(&dbgHdr,0,(sizeof dbgHdr));

      // Fill in the remaining parts of the header of the DBG PE file.
      dbgHdr.Signature          = IMAGE_SEPARATE_DEBUG_SIGNATURE;
      dbgHdr.Flags              = 0;
      dbgHdr.Machine            = ntHdrs.FileHeader.Machine;
      dbgHdr.Characteristics    = ntHdrs.FileHeader.Characteristics;
      dbgHdr.TimeDateStamp      = ntHdrs.FileHeader.TimeDateStamp;
      dbgHdr.CheckSum           = ntHdrs.OptionalHeader.CheckSum;
      dbgHdr.ImageBase          = ntHdrs.OptionalHeader.ImageBase;
      dbgHdr.SizeOfImage        = ntHdrs.OptionalHeader.SizeOfImage;
      dbgHdr.NumberOfSections   = ntHdrs.FileHeader.NumberOfSections;
      dbgHdr.ExportedNamesSize  = 0;
      dbgHdr.DebugDirectorySize = sizeof(IMAGE_DEBUG_DIRECTORY);
      dbgHdr.SectionAlignment   = ntHdrs.OptionalHeader.SectionAlignment;

      // Write the header of DBG PE file.
      dbgFile.write(dbgHdr);

   }

   // Copy the Section table (a sequence of IMAGE_SECTION_HEADER), if
   // required, and create the contents for the Code View sstSegMap to
   // be written later on.
   std::vector<CVssSegMapSegDesc> segMap;
   for(DWORD i=0 ; i<(DWORD)ntHdrs.FileHeader.NumberOfSections ; ++i) {
      IMAGE_SECTION_HEADER secHdr;
      if(1 != fread(&secHdr,(sizeof secHdr),1,ef)) {
         throw std::runtime_error("Could not read SECTION_HEADER"
                                  " from exe file");
      }
      if(!bareCodeView) {
         dbgFile.write(secHdr);
      }

      CVssSegMapSegDesc segDesc =
         {0,0,0,((WORD) i),0xFFFF,0xFFFF,0,((WORD) secHdr.Misc.VirtualSize)};
      segMap.push_back(segDesc);
   }

   // Note: We do currently not attempt to create the
   // IMAGE_DEBUG_TYPE_OMAP_FROM_SRC and IMAGE_DEBUG_TYPE_OMAP_TO_SRC
   // subsections.

   // Write the IMAGE_DEBUG_DIRECTORY to the DBG PE file.
   IMAGE_DEBUG_DIRECTORY dbgDir;
   memset(&dbgDir,0,(sizeof dbgDir));

   // Skips this if bare Code View output is requested (i.e. not an DBG PE file)
   if(!bareCodeView) {
      dbgDir.Characteristics  = 0;
      dbgDir.TimeDateStamp    = ntHdrs.FileHeader.TimeDateStamp;
      dbgDir.MajorVersion     = 0;
      dbgDir.MinorVersion     = 0;
      dbgDir.Type             = IMAGE_DEBUG_TYPE_CODEVIEW;
      dbgDir.SizeOfData       = 0; // Will be filled in later
      dbgDir.AddressOfRawData = 0;
      dbgDir.PointerToRawData = sizeof(IMAGE_SEPARATE_DEBUG_HEADER)+
         (ntHdrs.FileHeader.NumberOfSections * sizeof(IMAGE_SECTION_HEADER))+
         sizeof(IMAGE_DEBUG_DIRECTORY);
      dbgFile.write(dbgDir);
   }

   // Remember where to write the Code View header - this is lfaBase
   unsigned int lfaBase = dbgFile.tell();

   // Reserve space for the Code View header
   dbgFile.skip(sizeof(CVHdr));

   // Read the CodeView header of the TDS file - the first record in the file.
   CVHdr tdsCvHdr;
   tdsFile.seek(0);
   tdsFile.read(tdsCvHdr);

   mylog()(2) << "TDS header with signature "
              << tdsCvHdr.signature[0]
              << tdsCvHdr.signature[1]
              << tdsCvHdr.signature[2]
              << tdsCvHdr.signature[3] << std::endl;

   if('N' == tdsCvHdr.signature[0] &&
      'B' == tdsCvHdr.signature[1] &&
      '0' == tdsCvHdr.signature[2] &&
      '9' == tdsCvHdr.signature[3]) {

      // TDS file already contains Code View NB09 - no need for conversion,
      // just do a plain copy and update the PE header afterwards.
      dbgFile.write(tdsCvHdr);
      copyFile(dbgFile,tdsFile);

   } else if('F' == tdsCvHdr.signature[0] &&
             'B' == tdsCvHdr.signature[1] &&
             '0' == tdsCvHdr.signature[2] &&
             ('9' == tdsCvHdr.signature[3] ||
              'A' == tdsCvHdr.signature[3])) {

      // Do the conversion and update the PE header afterwards.
      convertTdsToCodeView(conv,tdsCvHdr,tdsFile,segMap,dbgFile,lfaBase);

   } else {
      throw std::runtime_error("Unidentified TDS signature");
   }

   // Are we generating a PE based DBG file or just a bare Code View file?
   if(!bareCodeView) {
      // Compute the total amount of Code View data
      dbgDir.SizeOfData = dbgFile.tell() - lfaBase;

      // Write out the updated dbgDir.SizeOfData
      dbgFile.seek(sizeof(IMAGE_SEPARATE_DEBUG_HEADER)+
                   ntHdrs.FileHeader.NumberOfSections*
                   sizeof(IMAGE_SECTION_HEADER));
      dbgFile.write(dbgDir);
   }
}

int tds2dbg(const char*  exeFileName, 
            const char*  tdsFileName,
            const char*  dbgFileName,
            unsigned int verbosity,
            bool         doConvTypes,
            bool         staticsAsPublics,
            bool         bareCodeView)
{
   // Set the verbosity level.
   mylog().setVerbosity(verbosity);

   int ret = 1;

   FILE *ef = fopen(exeFileName,"rb+");
   if(ef) {
      FILE *tf = fopen(tdsFileName,"rb");
      if(tf) {
         FILE* df = fopen(dbgFileName,"wb");
         if(df) {
            try {
               genDbgFromExeNTds(ef,tf,df,doConvTypes,staticsAsPublics,
                                 bareCodeView);
            } catch(std::exception& e) {
               mylog()(0) << e.what() << std::endl;
            }
            fclose(df);
            ret = 0;
         } else {
            mylog()(0)
               << "Unable to open " << dbgFileName << " for writing" << std::endl;
         }
         fclose(tf);
      } else {
         mylog()(0)
            << "Unable to open " << tdsFileName << " for reading" << std::endl;
      }
      fclose(ef);
   }
   return ret;
}


// Local Variables:
// mode:c++
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
