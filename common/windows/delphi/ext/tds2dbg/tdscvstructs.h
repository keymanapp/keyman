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

/// @file tdscvstruct.sh TDS and Code View structures.

#ifndef TDSCVSTRUCTS_H
#define TDSCVSTRUCTS_H

#include <windows.h>

// Use word-alignment
#ifdef __BORLANDC__
#pragma option push
#pragma option -a2 -w-8059
#endif

/// @page DebugSymFileStructures Debug Symbol File Structure
///
/// @section DBGFileStructure DBG File Structure.
///
/// Resources
/// <ul>
///  <li>Under The Hood, "The Scoop on .DBG Files", Matt Pietrek, 
///      Microsoft Systems Journal, March 1999</li>
///  <li>Undocumented Windows 2000 Secrets, A Programers Cookbook,
///      Sven B. Schreiber, Addison-Wesley,
///      (http://undocumented.rawol.com/)</li>
///  <li>Tool Interface Standard (TIS) -
///      Formats Specification for Windows Version 1.0
///      (http://www.x86.org/ftp/manuals/tools/winf10.pdf)</li>
/// </ul>
/// 
/// See the documentation for IMAGE_SEPARATE_DEBUG_HEADER in winnt.h:
/// @code
/// IMAGE_SEPARATE_DEBUG_HEADER
///  Signature = IMAGE_SEPARATE_DEBUG_SIGNATURE
/// IMAGE_SECTION_HEADER* (Copied from the executable)
/// IMAGE_DEBUG_DIRECTORY* (COFF, MISC, FPO Data, CodeView)
/// COFF Information       - not generated
/// Misc Information       - not generated
/// FPO Data               - not generated
/// CodeView Information   - The only information handled by MS Debuggers
///  lfaBase: NB09 (ascii)
///  DWORD offsetOfSubSectionDir;
///  SubSectionTable
///   SubSection*
///  SubSectionDir
///   SubSectionDirEntry*
/// @endcode
///
///
/// @section PDBFileStructure PDB File Structure
///
/// NOTE: The current code is not able to generate PDB files.
///
/// A PDB file is a PE file (see
/// "http://msdn.microsoft.com/en-us/magazine/ms809762.aspx"
/// MSDN Magazine
/// Debugging and Error Handling Technical Articles
/// Peering Inside the PE: A Tour of the Win32 Portable Executable File Format,
/// By Matt Pietrek, March 1994)
///
/// PE file structure (see winnt.h).
/// @code
///  IMAGE_DOS_HEADER
///    e_lfanew = Releative location of IMAGE_NT_HEADERS
///  IMAGE_NT_HEADERS
///   DWORD Signature = IMAGE_NT_SIGNATURE
///   IMAGE_FILE_HEADER
///     Machine
///     NumberOfSections
///   IMAGE_OPTIONAL_HEADER
///     IMAGE_DATA_DIRECTORY[16]
///      DWORD VirtualAddress
///      DWORD Size
///  Section Table
///   IMAGE_SECTION_HEADER
///
/// IMAGE_DATA_DIRECTORY[IMAGE_DIRECTORY_ENTRY_DEBUG] points to:
///   IMAGE_DEBUG_DIRECTORY+ (number of dirs computed from DataDirectory.Size)
///    Type=IMAGE_DEBUG_TYPE_CODEVIEW
/// @endcode

/// @page TdsFileStructure Structure of TDS file
///
/// CodeView header signature is "FB09" or "FB0A". Tail of tds file is
/// "FB0A" or "FB09" followed by length of file.
///
/// The layout of a TDS file seems to be (derived from a dump made by
/// Borland's tdump.exe):
///
/// @code
/// Header
/// sstModule*
///  sstAlignSym |* (sstAlignSym and sstSrcModule occur in pairs)
///  sstSrcModule|
/// sstGlobalSym
/// sstGlobalTypes
/// sstNames
/// @endcode
///
///
/// @section TDSsstNamesSubSec Borlands sstNames subsection
///
/// A Borland specific susbection with index 0x130.
///
/// @code
/// struct TDSsstNames {
///   DWORD numberOfNames
///   Name [];
/// };
///
/// struct Name {
///   unsigned char len;  // Length of name excluding zero-terminator
///   char          name; // Zero-terminated name
/// };
/// @endcode
///
///
/// @section TDSsstSrcModuleSubSec Borlands sstSrcModule subsection
///
/// The only deviation is the use of name index in the file info record
/// instead of an inlined name:
///
/// @code
/// struct TDSssSrcModule {
///   CVssSrcModuleHdr  hdr;
///   TDSssSrcModuleRec files[hdr.cFile];
/// };
///
/// struct CVssSrcModule {
///   CVssSrcModuleHdr  hdr;
///   CVssSrcModuleRec  files[hdr.cFile];
/// };
///
/// struct CVssSrcModuleHdr : public CVssSrcModuleFixedHdr {
///   DWORD baseSrcFile[cFile];
///   DWORD startEnd[2*cSeg];
///   WORD  seg[cSeg];
/// };
///
/// struct CVssSrcModuleFixedHdr {
///   WORD  cFile;
///   WORD  cSeg;
/// };
///
/// struct TDSssSrcModuleRec {
///   TDSsrcModuleFile  file;
///   CVsrcModuleSeg    segs[file.cSeg];
/// };
///
/// struct CVssSrcModuleRec {
///   CVsrcModuleFile   file;
///   CVsrcModuleSeg    segs[file.cSeg];
/// };
///
/// struct TDSssSrcModuleFile : public TDSssSrcModuleFileHdr {
///   DWORD  baseSrcLn[cSeg];
///   DWORD  startEnd[2*cSeg];
/// };
///
/// struct TDSssSrcModuleFileHdr {
///   WORD   cSeg;
///   DWORD  NameId; // This is "WORD pad" in MS STI
/// };
///
/// struct CVssSrcModuleFile : public CVssSrcModuleFileHdr {
///   DWORD  baseSrcLen[cSeg];
///   DWORD  startEnd[2*cSeg];
///   WORD   nameLen;
///   char   name[nameLen];
/// };
///
/// struct CVssSrcModuleFileHdr {
///   WORD   cSeg;
///   WORD   pad;
/// };
///
/// struct CVssSrcModuleSeg {
///   CVssSrcModuleSegHdr hdr;
///   DWORD               offset[cPair];
///   WORD                linenumber[cPair];
/// };
///
/// struct CVssSrcModuleSegHdr {
///   WORD   seg;
///   WORD   cPair;
/// };
/// @endcode
///
///
/// @section TDSsstSegMapSubSec Code View sstSegMap subsection
///
/// Not present in TDS files. But apparently required (and ignored?) by
/// Microsofts debuggers.
///
//
/// @section TDSsstGlobalSymSubSec TDS sstGlobalSym subsection
///
/// The header (TDSssGlobalSymHdr) is an extension of the corresponding
/// Code View header (CVssGlobalSymHdr).
/// 
///
/// @section TDSSymbolRecords TDS Symbol Records
///
///
/// @subsection TDSSymbolRecordEntryPoint The Entry Point Symbol Record
///
/// Identified by means of kind index S_ENTRY32=0x210. There is no
/// corresponding record in Code View. Payload:
///
/// @code
/// struct TDSsrEntry32Payload {
///   DWORD offset; // Offset
///   WORD  seg;    // Segment
/// };
/// @endcode
///
///
/// @subsection TDSSymbolRecordNameSpace The Namespace Symbol Record
///
/// Identified by means of kind index S_NAMESPACE=0x25. There is no
/// corresponding record in Code View. Payload:
///
/// @code
/// struct TDSsrNameSpacePayload {
///   DWORD nameIdx;  // Index of namespace name in sstNames.
///   DWORD pad;      // Padding?
///   WORD  cUsing;   // Number of name indices for using namespaces.
///   DWORD [cUsing]; // Indices of using namespace names in sstNames.
/// };
/// @endcode
///
///
/// @subsection TDSSymbolRecordUDF The User Defined Type Symbol Record
///
/// Identified by means of kind index s_UDT=0x4. The TDS form of the
/// record payload (TDSsrUserDefinedTypePayload) deviates from the Code View
/// form of the record payload (CVsrUserDefinedTypePayload):
///
/// @code
/// struct CVsrUserDefinedTypePayloadHdr {
///   WORD  typeIdx;      // Type index
/// };
///
/// struct CVsrUserDefinedTypePayload {
///   CVsrUserDefinedTypePayloadHdr hdr;
///   unsigned char                 nameLen;
///   char                          name[nameLen];
/// };
///
/// struct TDSsrUserDefinedTypePayload {
///   DWORD typeIdx;      // Type index
///   WORD  flags;        // 0: on=tag,off=typedef 1: on=nested
///   DWORD nameIdx;      // Index of the types name in sstNames.
///   DWORD brwsInfoOffs; // Browser info offset
/// };
/// @endcode
///
///
/// @subsection TDSSymbolRecordGlobalProcRef The Global Procedure Reference Symbol Record
///
/// Identified by means of kind index S_GPROCREF=0x20. There is no
/// corresponding record in Code View --- except, perhaps, for
/// S_PROCREF. The S_GPROCREF symbol can be used to identify public
/// functions/procedures that should be registered in
/// sstGlobalPub of the DBG file. Payload:
///
/// @code
/// struct TDSsrGlobalProcRefPayload {
///   DWORD offSymRef;      // Seems always to be 0.
///   DWORD typeIdx;        // Type index. 0 if none.
///   DWORD nameIdx;        // Index of name in sstNames
///   DWORD brwsInfoOffset; // Browser info offset?
///   DWORD offset;         // Address offset of proc or name index of
///                         // DLL if segment==0xFFFF.
///   WORD  segment;        // Segment.
///   DWORD nameIdx;        // Index of linker name, if segment=0xFFFF?
/// };
/// @endcode
///
///
/// @subsection TDSSymbolRecordGlobalDataRef The Global Data Reference Symbol Record
///
/// Identified by means of kind index S_GDATAREF=0x21. There is no
/// corresponding record in Code View --- except, perhaps, for
/// S_DATAREF. The S_GDATAREF symbol can be used to identify public
/// data that should be registered in sstGlobalPub of the DBG
/// file. Payload:
///
/// @code
/// struct TDSsrGlobalDataRefPayload {
///   DWORD offSymRef;      // Seems always to be 0.
///   DWORD typeIdx;        // Type index. 0 if none.
///   DWORD nameIdx;        // Index of name in sstNames
///   DWORD brwsInfoOffset; // Browser info offset?
///   DWORD offset;         // Address offset of proc or name index of
///                         // DLL if segment==0xFFFF.
///   WORD  segment;        // Segment.
/// };
/// @endcode
///
///
/// @subsection TDSSymbolRecordGlobalData The Global Data Symbol Record
///
/// Identified by means of kind index S_GLOBALDATA=0x202. The TDS form
/// of the payload (TDSsrGlobalDataPayload) deviates from the Code
/// View form of the payload (CVsrGlobalDataPayload):
///
/// @code
/// struct CVsrGlobalDataPayloadHdr {
///   DWORD         offset;
///   WORD          segment;
///   WORD          typeIdx;
/// };
/// 
/// struct CVsrGlobalDataPayload {
///   CVsrGlobalDataPayloadHdr hdr;
///   unsigned char            nameLen;
///   char                     name[nameLen];
/// };
///
/// struct TDSsrGlobalDataPayload {
///   DWORD         offset;       // Address offset
///   WORD          segment;      // Segment
///   WORD          flags;        // 1=TLS var, 0=Not a TLS var.?
///   DWORD         typeIdx;      // Index of type
///   DWORD         nameIdx;      // Index of name in sstNames.
///   DWORD         brwsInfoOffs; // Browser info offset?
/// };
/// @endcode
///
///
/// @subsection TDSSymbolRecordLocalData The Local Data Symbol Record
///
/// Identified by means of kind index S_LOCALDATA=0x201. The TDS form
/// of the payload is identical to TDSsrGlobalDataPayload. The Code View
/// form of the payload is identical to CVsrGlobalDataPayload.
///
///
/// @section TDSsstGlobalTypesSubSec TDS sstGlobalTypes subsection
///
/// TDS relies on a signature of 0x1 in the first 4 bytes, whereas
/// Code View uses 0x1000000. In TDS files, offsets in type directory
/// are computed relative to the start of the subsection, whereas for
/// Code View (NB09), offsets are relative to the first type record.
///
///
/// @section TDSTypeRecords The TDS Type Records
///
/// TDS and Code View rely on the same type record header (CVtrHeader).
/// The header is followed by a sequence of one or more type strings as
/// defined in the following subsections.
///
///
/// @subsection TDSTypeStringModifier The Modifier Type String
///
/// Identified by LF_MODIFIER=0x1. Code View uses the format specified
/// by CVtsModifier, whereas TDS files use the format specified by
/// TDStsModifier.
///
///
/// @subsection TDSTypeStringPointer The Pointer Type String
///
/// Identified by LF_POINTER=0x2. Code view relies on the format
/// specified by CVtsPointer, whereas TDS files rely on the format
/// specified by TDStsPointer. In both cases, there is a variant part
/// that follows immediately after CVtsPointer and TDStsPointer, respectively.
///
/// @todo Investigate the differences in the variant part.
///
///
/// @subsection TDSTypeStringArray The Array Type String
///
/// Identified by LF_ARRAY=0x3. Code view use the format specified by
/// CVtsArray followed by a length-prefixed name of the array. TDS
/// files rely on a format specified by TDStsArray.
///
///
/// @subsection TDSTypeStringClass The Class Type String
///
/// Identified by LF_CLASS=0x4. The same comments as for the structure
/// type string (LF_STRUCTURE) applies here as well.
///
///
/// @subsection TDSTypeStringStructure The Structure Type String
///
/// Identified by LF_STRUCTURE=0x5. It deviates from the corresponding Code
/// View structure:
///
/// @code
/// struct CVtsClasses {
///   WORD count;     // Number of elements in the class or structure.
///   WORD fieldIdx;  // Type index of the field list for this class.
///   WORD property;
///   WORD dList;     // Type index of the derivation list.
///   WORD vshape;    // Type index of the virtual function table shape desc.
///   unsigned char len[]; // Numeric leaf describing the size of the structure.
///   unsigned char nameLen;
///   char          name[nameLen];
/// };
///
/// struct TDStsClasses {
///   WORD  count;    // Number of elements in the class or structure
///   DWORD fieldIdx; // Type index of the field list for this class.
///   WORD  property;
///   DWORD contCls;  // Type index of containing class.
///   DWORD dList;    // Type index of the derivation list.
///   DWORD vshape;   // Type index of the virtual function table shape desc.
///   DWORD nameIdx;  // Index of name in sstNames
///   WORD  len;      // Length of the structure in bytes
/// };
/// @endcode
///
/// Apart from this, bit 8 of TDStsClasses::property is on if the class has a
/// destructor, whereas bit 8 of CVtsClasses::property indicates whether the
/// definition is scoped.
///
///
/// @subsection TDSTypeStringEnum The Enum Type String
///
/// Identified by LF_ENUM=0x7. It deviates from the corresponding Code View
/// structure as follows:
///
/// @code
/// struct CVtsEnum {
///   WORD count;    // Number of enumerations.
///   WORD typeIdx;  // Type index of the container underlying the enum.
///   WORD fieldIdx; // Type index of the fields of the enum.
///   WORD property; // Property bit field
///   unsigned char nameLen;
///   char          name[nameLen];
/// };
///
/// struct TDStsEnum {
///   WORD count;     // Number of enumeration.
///   DWORD typeIdx;  // Type index of the container underlying the enum.
///   DWORD fieldIdx; // Type index of the fields of the enum
///   DWORD contCls;  // Type index of the containing class
///   DWORD nameIdx;  // Index of name in sstNames
/// };
/// @endcode
///
///
/// @subsection TDSTypeStringProcedure The Procedure Type String
///
/// Identified by LF_PROCEDURE=0x8. The Code View and TDS structures
/// (CVtsProcedure and TDStsProcedure) dffer in the size of the
/// argList member.
///
///
/// @subsection TDSTypeStringMembFunc The Member Function Type String
///
/// Identified by LF_MFUNCTION=0x9. It deviates from the corresponding
/// Code View structure as follows:
///
/// @code
/// struct CVtsMFunction {
///   WORD  rvTypeIdx;  // Index of the return value type.
///   WORD  classIdx;   // Type index of the containg class.
///   WORD  thisIdx;    // Type index of the this pointer.
///   BYTE  callConv;   // Calling convention.
///   BYTE  reserved;
///   WORD  parms;      // Number of params.
///   WORD  argList;    // Type index of the argument list.
///   DWORD thisAdjust; // This pointer adjustment.
/// };
///
/// struct TDStsMFunction {
///   DWORD rvTypeIdx;  // Index of the return value type.
///   DWORD classIdx;   // Type index of the containing class.
///   DWORD thisIdx;    // Type index of the this pointer.
///   BYTE  callConv;   // Calling convention.
///   BYTE  reserved;
///   WORD  params;     // Number of params.
///   DWORD argList;    // Type index of the argument list.
///   DWORD thisAdjust; // This pointer adjustment.
/// };
/// @endcode
///
///
/// @subsection TDSTypeStringVTShape The Virtual Function Table Shape Type String
///
/// Identified by LF_VTSHAPE=0xA. It is identical to the corresponding Code
/// View structure.
///
///
/// @subsection TDSTypeStringLabel The Label Type String
///
/// Identified by LF_LABEL=0xE. It is identical to the corresponding Code
/// View structure.
///
///
/// @subsection TDSTypeStringPArray The PArray Type String
///
/// Identified by LF_PARRAY=0x32. There is no corresponding Code View structure.
///
/// @code
/// struct TDStsPArray {
///   DWORD elemTypeIdx; // Type index of type of array elements
///   DWORD idxTypeIdx;  // Type index of index variable
///   DWORD nameIdx;     // Name index in sstNames
///   WORD  size;
///   WORD  numElements;
/// };
/// @endcode
///
///
/// @subsection TDSTypeStringArgList The ArgList Type String
///
/// Identified by LF_ARGLIST=0x0201. It is identical to the
/// corresponding Code View structure.
///
///
/// @subsection TDSTypeStringFieldList The Field List Type String
///
/// Identified by LF_FIELDLIST=0x204. It is identical to the corresponding
/// Code View structure. The padding used between the individual fields are
/// as defined for Code View, too.
///
///
/// @subsection TDSTypeStringMethodList The Method List Type String
///
/// Identified by LF_MLIST=0x207. It is deviates from the corresponding
/// Code View structure.
///
/// @code
/// struct CVtsMList {
///   CVtsMListElem records[];
/// };
///
/// struct CVtsMListElem {
///   WORD          attribute;
///   WORD          typeIdx;      // Type index of the procedure record
///   DWORD         vtabOffset[]; // 0 or 1 element.
/// };
///
/// struct TDStsMList {
///   TDStsMListElem records[];
/// };
///
/// struct TDStsMListElem {
///   WORD           attribute;
///   DWORD          typeIdx;
///   DWORD          brwsInfoOffst; // Browser info offset
///   DWORD          vtabOffset[];; // 0 or 1 element.
/// };
/// @endcode
///
///
/// @subsection TDSTypeStringEnumerate The Enumerate Type String
///
/// Identified by LF_ENUMERATE=0x403. It deviates from the corresponding
/// Code View structure:
///
/// @code
/// struct CVtsEnumerate {
///   WORD          attribute;
///   unsigned char value[]; // Numeric leaf specifying the value of the enum
///   unsigned char nameLen;
///   char          name[nameLen]; 
/// };
///
/// struct TDStsEnumerator {
///  WORD   attribute;
///  DWORD  nameIdx;        // Index of the enum name in sstNames
///  DWORD  brwsInfoOffs;   // Browser info offset?
///  unsigned char value[]; // Numeric leaf specifying the value of the enum
/// };
///
///
/// @subsection TDSTypeStringMethod The Method Type String
///
/// Identified by LF_METHOD=0x408. It deviates from the corresponding Code View
/// structure:
///
/// @code
/// struct CVtsMethod {
///   WORD count;   // Number of methods in method list
///   WORD listIdx; // Type index of the method list (size>1 when overloaded).
///   BYTE nameLen;
///   char name[nameLen];
/// };
///
/// struct TDStsMethod {
///   WORD count;    // Number of methods in method list
///   DWORD listIdx; // Type index of the method list (size_1 when overloaded).
///   DWORD nameIdx; // Index of name in sstNames
/// };
/// @endcode


/// @brief Code View header
struct CVHdr {
   char  signature[4]; // File offset of this field is lfaBase
   DWORD lfoDir; 
};

/// @brief Code View directory header
struct CVDirHdr {
   WORD  cbDirHdr;   // Length of directory header
   WORD  cbDirEntry; // Length of each directory entry
   DWORD cDir;       // Number of directory entries
   LONG  lfoNextDir; // Offset of lfaBase of next dir. Unused in TDS files.
   DWORD flags;      // Unused.
};

/// @brief Code View directory entry
struct CVDirEntry {
   WORD  subsection; // Subdirectory index.
   WORD  iMod;       // Module index.
   DWORD lfo;        // Offset from the base address lfaBase
   DWORD cb;         // Number of bytes in subsection
};

/// @brief Extended Code View subsection types for TDS files
enum CVSst
{
   CVsstIdxModule      = 0x120,
   CVsstIdxAlignSym    = 0x125,
   CVsstIdxSrcModule   = 0x127,
   CVsstIdxGlobalSym   = 0x129,
   CVsstIdxGlobalPub   = 0x12a,
   CVsstIdxGlobalTypes = 0x12b,
   CVsstIdxSegMap      = 0x12d,
   CVsstIdxNames       = 0x130,
   CVsstIdxStaticSym   = 0x134
};

/// @brief Header of Code View sstModule subsection
///
/// The header is followed by a length-prefixed name of the module
/// and cSeg CVssModuleSegInfo records (in that order).
struct CVssModuleHdr {
   WORD  ovlNumber;
   WORD  iLib;
   WORD  cSeg;
   WORD  style;
};

/// @brief Header of TDS sstModule subsection
///
/// The header is followed by cSeg CVssModuleSegInfo records.
struct TDSssModuleHdr : public CVssModuleHdr {
   DWORD nameIdx; ///< 1-based name index in sstNames
   char  pad[16]; ///< Unused?
};

/// @brief Segment info record from Code View sstModule subsection
struct CVssModuleSegInfo {
   WORD    seg;
   WORD    flags;
   DWORD   offs;
   DWORD   cbSeg;
};

/// @brief Fixed part of the header of the Code View and TDS
///        sstSrcModule subsection.
///
/// The header is followed by a structure of the form
/// @code
///   DWORD baseSrcFile[cFile];
///   DWORD startEnd[2*cSeg];
///   WORD  seg[cSeg];
/// @endcode
/// For DBG Files, the above is followed by cFile instances of the
/// following structure
/// @code
///   WORD  cSeg2; // See CVssSrcModuleFileHdr
///   WORD  pad;
///   DWORD baseSrcLen[cSeg2];
///   DWORD startEnd[2*cSeg2];
///   WORD  nameLen;
///   char  name[nameLen];
///   {
///     WORD seg; // See CVssSrcModuleSegHdr
///     WORD cPair;
///     DWORD offset[cPair];
///     WORD  linenumber[cPair];
///   } [cSeg2];
/// @endcode
/// whereas for TDS files, it is followed by cFile instances of the
/// following structure
/// @code
///   WORD  cSeg2; // See TDSssSrcModuleFileHdr
///   DWORD nameIdx;
///   DWORD baseSrcLen[cSeg2];
///   DWORD startEnd[2*cSeg2];
///   {
///     WORD seg; // Se CVssSrcModuleSegHdr
///     WORD cPair;
///     DWORD offset[cPair];
///     WORD  linenumber[cPair];
///   } [cSeg2];
/// @endcode
struct CVssSrcModuleFixedHdr {
   WORD  cFile;
   WORD  cSeg;
};

/// @brief Header of the Segment info record from Code View sstSrcModule subsec
struct CVssSrcModuleSegHdr {
   WORD seg;
   WORD cPair;
};

/// @brief Header of the File info record from TDS sstSrcModule subsec
struct TDSssSrcModuleFileHdr {
   WORD  cSeg;
   DWORD nameIdx;
};

/// @brief Header of the File info record from Code View sstSrcModule subsec
struct CVssSrcModuleFileHdr {
   WORD  cSeg;
   WORD  pad;
};

/// @brief Header of the Code View sstSegMap subsection
struct CVssSegMapHdr {
   WORD cSeg;    ///< Number of segment descriptors
   WORD cSegLog; ///< Number of logical segment descriptors
};

/// @brief A segment descriptor in the Code View sstSegMap subection
struct CVssSegMapSegDesc {
   WORD flags;      ///< Descriptor flags.
   WORD ovl;        ///< Logical overlay number.
   WORD group;      ///< Group index.
   WORD frame;      ///< Frame.
   WORD iSegName;   ///< Segment name. 0xFFFF means no name.
   WORD iClassName; ///< Class name. 0xFFFF means no name.
   WORD offset;     /** Offset of logical segment within physical segment.
                        Always 0. */
   WORD cbSeg;      ///< Size of segment.
};

/// @brief Header of the Code View sstGlobalTypes subsection
struct CVssGlobalTypesHdr {
   DWORD flags;  ///< Flags and signature.
   DWORD cTypes; ///< Number of types in the subsection
};

/// @brief Header of the Code View sstGlobalSym subsection
struct CVssGlobalSymHdr {
   WORD  symhash;
   WORD  addrhash;
   DWORD cbSymbol;
   DWORD cbSymHash;
   DWORD cbAddrHash;
};

/// @brief Header for the TDS sstGlobalSym subsection
struct TDSssGlobalSymHdr : public CVssGlobalSymHdr {
   DWORD  numUserDefTypes; ///< Number of user defined types.
   DWORD  numOtherSyms;    ///< Number of other symbols.
   DWORD  numSyms;         ///< Number of symbols.
   DWORD  numNameSpcs;     ///< Number of namespaces.
};

/// @brief Header for the CodeView and TDS sstAlignSym subsection
struct CVssAlignSymHdr {
   DWORD  signature; ///< Always 1.
};

/// @brief Code View leaf identifiers
enum CVLeafId {
   LF_MODIFIER   = 0x0001,
   LF_POINTER    = 0x0002,
   LF_ARRAY      = 0x0003,
   LF_CLASS      = 0x0004,
   LF_STRUCTURE  = 0x0005,
   LF_PROCEDURE  = 0x0008,
   LF_MFUNCTION  = 0x0009,
   LF_NOTTRANS   = 0x0010,
   LF_PAD0       = 0x00f0,
   LF_PAD1       = 0x00f1,
   LF_PAD2       = 0x00f2,
   LF_PAD3       = 0x00f3,
   LF_PAD4       = 0x00f4,
   LF_PAD5       = 0x00f5,
   LF_PAD6       = 0x00f6,
   LF_PAD7       = 0x00f7,
   LF_PAD8       = 0x00f8,
   LF_PAD9       = 0x00f9,
   LF_PAD10      = 0x00fa,
   LF_PAD11      = 0x00fb,
   LF_PAD12      = 0x00fc,
   LF_PAD13      = 0x00fd,
   LF_PAD14      = 0x00fe,
   LF_PAD15      = 0x00ff,
   LF_ARGLIST    = 0x0201,
   LF_MLIST      = 0x0207,
   LF_NUMERIC    = 0x8000,
   LF_CHAR       = 0x8000,
   LF_SHORT      = 0x8001,
   LF_USHORT     = 0x8002,
   LF_LONG       = 0x8003,
   LF_ULONG      = 0x8004,
   LF_REAL32     = 0x8005,
   LF_REAL64     = 0x8006,
   LF_REAL80     = 0x8007,
   LF_REAL128    = 0x8008,
   LF_QUADWORD   = 0x8009,
   LF_UQUADWORD  = 0x800a,
   LF_REAL48     = 0x800b,
   LF_COMPLEX32  = 0x800c,
   LF_COMPLEX64  = 0x800d,
   LF_COMPLEX80  = 0x800e,
   LF_COMPLEX128 = 0x800f,
   LF_VARSTRING  = 0x8010
};

/// @brief Header of the Code View type record.
struct CVtrHeader {
   WORD len;     ///< Length excluding the length field itself.
};

/// @brief Type string for Code View LF_MODIFIER
struct CVtsModifier {
   WORD  attribute;  ///< Modifier attributes
   WORD  typeIdx;    ///< Index of modified type
};

/// @brief Type string for TDS LF_MODIFIER
struct TDStsModifier {
   WORD  attribute;  ///< Modifier attributes
   DWORD typeIdx;    ///< Index of modified type
};

/// @brief Type string for Code View LF_POINTER
struct CVtsPointer {
   WORD  attribute; ///< Pointer attributes.
   WORD  typeIdx;   ///< Type index of the object pointed to.
};

/// @brief Type string for TDS LF_POINTER
struct TDStsPointer {
   WORD  attribute; ///< Pointer attributes.
   DWORD typeIdx;   ///< Type index of the object pointed to.
};

/// @brief Type string for Code View LF_ARRAY.
///
/// The complete type string consists of the CVtsArray structure followed
/// by a length-prefixed name.
struct CVtsArray {
   WORD  elemTypeIdx; ///< Type index of the array elements
   WORD  idxTypeIdx;  ///< Type index of the index variable
};

/// @brief Type string for TDS LF_ARRAY
struct TDStsArray {
   DWORD elemTypeIdx; ///< Type index of the array elements
   DWORD idxTypeIdx;  ///< Type index of the index variable
   DWORD nameIdx;
   WORD  size;
   WORD  numElements;
};

/// @brief Type string for Code View LF_CLASS (and LF_STRUCTURE)
///
/// The complete type string consists of the CVtsClass structure followed
/// by a numeric leaf specifying the size of the class in bytes and the
/// length-prefixed name of the type (in that order).
struct CVtsClass {
   WORD count;        ///< Number of elements in the class or structure.
   WORD fieldTypeIdx; ///< Type index of the field list of this class.
   WORD property;     ///< Property bit field.
   WORD dList;        ///< Type index of derivation list.
   WORD vShape;       ///< Type index of the virtual function table shape desc.
};

/// @brief Type string for TDS LF_CLASS (and LF_STRUCTURE)
///
/// The complete type string consists of the CVtsClass structure followed
/// by a numeric leaf specifying the size of the class in bytes.
struct TDStsClass {
   WORD  count;       ///< Number of elements in the class or structure.
   DWORD fieldTypeIdx;///< Type index of the field list of this class.
   WORD  property;    ///< Property bit field.
   DWORD cClass;      ///< Index of containing class.
   DWORD dList;       ///< Type index of derivation list.
   DWORD vShape;      ///< Type index of the virtual function table shape desc.
   DWORD nameIdx;     ///< Index of class name.
};

/// @brief Type string for Code View LF_PROCEDURE
struct CVtsProcedure {
   WORD rvTypeIdx;    ///< Type index of the return value.
   BYTE callConv;     ///< Calling convention.
   BYTE reserved;     ///< Reserved.
   WORD parms;        ///< Number of parameters.
   WORD argList;      ///< Type index of argument list.
};

/// @brief Type string for TDS LF_PROCEDURE
struct TDStsProcedure {
   DWORD rvTypeIdx;   ///< Type index of the return value.
   BYTE  callConv;    ///< Calling convention.
   BYTE  reserved;    ///< Reserved.
   WORD  parms;       ///< Number of parameters.
   DWORD argList;     ///< Type index of argument list.
};

/// @brief Type string for Code View LF_MFUNCTION
struct CVtsMFunction {
   WORD  rvTypeIdx;   ///< Type index of the return value.
   WORD  classIdx;    ///< Type index of the containing class.
   WORD  thisIdx;     ///< Type index of the this parameter.
   BYTE  callConv;    ///< Calling convention.
   BYTE  reserved;    ///< Reserved.
   WORD  parms;       ///< Number of parameters.
   WORD  argList;     ///< Type index of argument list.
   DWORD thisAdjust;  ///< Logical this adjuster.
};

/// @brief Type string for TDS LF_MFUNCTION
struct TDStsMFunction {
   DWORD rvTypeIdx;   ///< Type index of the return value.
   DWORD classIdx;    ///< Type index of the containing class.
   DWORD thisIdx;     ///< Type index of the this parameter.
   BYTE  callConv;    ///< Calling convention.
   BYTE  reserved;    ///< Reserved.
   WORD  parms;       ///< Number of parameters.
   DWORD argList;     ///< Type index of argument list.
   DWORD thisAdjust;  ///< Logical this adjuster.
};

/// @defgroup MemberAttrBitField Member Attribute Bit Field
/// @{

/// @brief Mask for the access attribute
#define MEMBATTR_ACCESS_MASK  0x0003

/// @brief Value of the access attribute if the method has no access protection
#define MEMBATTR_NOACCPROT    0x0000

/// @brief Value of the access attribute if the method offers private access
#define MEMBATTR_PRIVATE      0x0001

/// @brief Value of the access attribute if the method offers protected access
#define MEMBATTR_PROTECTED    0x0002

/// @brief Value of the access attribute if the method offers public access
#define MEMBATTR_PUBLIC       0x0003

/// @brief Mask for the method properties
#define MEMBATTR_PROP_MASK    0x001C

/// @brief Value of the method properties for a vanilla method
#define MEMBATTR_PROP_VANILLA 0x0000

/// @brief Value of the method properties for a virtual method
#define MEMBATTR_PROP_VIRTUAL 0x0004

/// @brief Value of the method properties for a static method
#define MEMBATTR_PROP_STATIC  0x0008

/// @brief Value of the method properties for a friend method
#define MEMBATTR_PROP_FRIEND  0x000C

/// @brief Value of the method properties for an "introducing" virtual method
#define MEMBATTR_PROP_INTVIRT 0x0010

/// @brief Value of the method properties for a pure virtual method
#define MEMBATTR_PROP_PUREV   0x0014

/// @brief Value of the method properties for an "introducing" pure virtual
///        method.
#define MEMBATTR_PROP_INTPURE 0x0018

/// @brief Set if method is never instantiated by the compiler.
#define MEMBATTR_PSEUDO       0x0020

/// @brief Set if class cannot be inherited
#define MEMBATTR_NONINHERIT   0x0040

/// @brief Set if class cannot be constructed
#define MEMBATTR_NOCONSTRUCT  0x0080

/// @brief Set if this is an overloaded operator
#define MEMBATTR_OVLOP        0x0100

/// @brief Set if this is a type conversion operator
#define MEMBATTR_CNVOP        0x0200

/// @brief Set if this is a constructor
#define MEMBATTR_ISCTOR       0x0400

/// @brief Set if this is a destructor
#define MEMBATTR_ISDTOR       0x0800

/// @brief Set if this is a compiler-created method
#define MEMBATTR_ISBLTIN      0x1000

/// @brief Set if this is a bitfield member
#define MEMBATTR_ISBITFIELD   0x2000

/// @}

/// @brief Type string element for Code View LF_MLIST
///
/// The full type string is a sequence of CVtsMListElem structures,
/// each optionally followed by a 4-byte vtaboffset.
struct CVtsMListElem {
   WORD  attribute;   ///< Member function attributes.
   WORD  typeIdx;     ///< Type index of member function signature.
};

/// @brief Type string element for TDS LF_MLIST
///
/// The full type string is a sequence of TDStsMListElem structures,
/// each optionally followed by a 4-byte vtaboffset.
struct TDStsMListElem {
   WORD  attribute;   ///< Member function attributes.
   DWORD typeIdx;     ///< Type index of member function signature.
   DWORD brwsOffs;    ///< Browser offset.
};

/// @brief Type string for TDS files.
struct TDSTypeString {
   WORD              leaf;       ///< Leaf identifier (see CVLeafId).
   union {
      TDStsModifier  modifier;   ///< Type string for a modifier
      TDStsPointer   pointer;    ///< Type string for a pointer.
      TDStsArray     array;      ///< Type string for an array.
      TDStsClass     cls;        ///< Type string for a class.
      TDStsProcedure procedure;  ///< Type string for a procedure.
      TDStsMFunction mFunction;  ///< Type string for a member function.
   };
};

/// @brief Type string for Code View files
struct CVTypeString {
   WORD              leaf;       ///< Leaf indentifier (see CVLeafId).
   union {
      CVtsModifier   modifier;   ///< Type string for a modifier.
      CVtsPointer    pointer;    ///< Type string for a pointer.
      CVtsArray      array;      ///< Type string for an array.
      CVtsClass      cls;        ///< Type string for a class.
      CVtsProcedure  procedure;  ///< Type string for a procedure.
      CVtsMFunction  mFunction;  ///< Type string for a member function.
   };
};

/// @brief The kind of symbol described in the payload of a symbol record.
enum CVSymKind {
   CVskCompile    = 0x0001,
   CVskRegister   = 0x0002,
   CVskUDT        = 0x0004,
   CVskSSearch    = 0x0005,
   CVskEnd        = 0x0006,
   CVskSkip       = 0x0007,
   CVskGProcRef   = 0x0020,
   CVskGDataRef   = 0x0021,
   CVskNamespace  = 0x0025,
   CVskUsing      = 0x0026,
   CVskBPRel      = 0x0200,
   CVskLocalData  = 0x0201,
   CVskGlobalData = 0x0202,
   CVskPublic     = 0x0203,
   CVskLocalProc  = 0x0204,
   CVskGlobalProc = 0x0205,
   CVskBlock32    = 0x0207,
   CVskEntry32    = 0x0210,
   CVskOptVar32   = 0x0211, // Variable life range support.
   CVskProcRet32  = 0x0212, // Procedure return epilogue.
   CVskSaveRegs32 = 0x0213, // ???
   CVskProcRef    = 0x0400,
   CVskDataRef    = 0x0401,
   CVskAlign      = 0x0402
};

/// @brief Header of the Code View symbol record
struct CVsymRecHdr {

   /// @brief Default constructor
   CVsymRecHdr() : len(1), symKind(CVskSkip) {}

   /// @brief Constructor
   CVsymRecHdr(WORD len_, WORD symKind_) : len(len_), symKind(symKind_) {}

   WORD  len;     ///< Length excluding length field itself
   WORD  symKind; ///< Kind of symbol (see CVSymKind)
};

/// @brief Header of the payload of the Code View and TDS Compile Flag
///        symbol record.
struct CVsrCompileFlagPayloadHdr {
   BYTE   machine;  ///< Target processor enum.
   BYTE   language; ///< Language enum.
   WORD   flags;    ///< Misc flags.
};

/// @brief Header of the payload of the Code View Register symbol record
///
/// @note Register tracking information is assumed to be absent.
struct CVsrRegisterPayloadHdr {
   WORD   typeIdx;  ///< Type index.
   WORD   regId;    ///< Register identifier.
};

/// @brief Payload of the TDS Register symbol record {
struct TDSsrRegisterPayload {
   DWORD  typeIdx;  ///< Type index.
   WORD   regId;    ///< Register indentifier.
   DWORD  nameIdx;  ///< Index of symbol name.
   WORD   brwsOffs; ///< Browser info offset.
};

/// @brief Header of the payload of the Code View User Defined Type
///        symbol record
struct CVsrUserDefinedTypePayloadHdr {
   WORD  typeIdx;      ///< Type index
};

/// @brief The payload of the TDS User Defined Type symbol record
struct TDSsrUserDefinedTypePayload {
   DWORD typeIdx;      ///< Type index
   WORD  flags;        ///< 0: on=tag,off=typedef 1: on=nested
   DWORD nameIdx;      ///< Index of the types name in sstNames
   DWORD brwsInfoOffs; ///< Browser info offset
};

/// @brief The payload of the Code View Start Search Symbol Record
struct CVsrStartSearchPayload {
   DWORD offs;         ///< Offset of first code within segment
   WORD  segment;      ///< Segment (PE section)
};

/// @brief The payload of the TDS Start Search Symbol Record
struct TDSsrStartSearchPayload : public CVsrStartSearchPayload {
   WORD  codeSyms;     ///< Number of code symbols.
   WORD  dataSyms;     ///< Number of data symbols.
   DWORD firstData;    ///< Offset of first data within segment
   WORD  dummy;        ///< Currently not understood what this is used for.
};

/// @brief The payload of the Code View Procedure Reference Record
struct CVsrProcRefPayload {
   DWORD checksum;     ///< Checksum.
   DWORD offSymRef;    /**< Offset of symbol record refered to relative to the
                            symbol table for the module (sstAlignSym). */
   WORD  module;       /**< Index of module that contains the symbol record for
                            the procedure. */
};

/// @brief The payload of the TDS Global Procedure Reference Record
struct TDSsrGlobalProcRefPayload {
  DWORD offSymRef;      /**< Offset of symbol record referred to relative to
                             the FB0A signature. Most often 0. */
  DWORD typeIdx;        ///< Type index. 0 if none.
  DWORD nameIdx;        ///< Index of name in sstNames
  DWORD brwsInfoOffset; ///< Browser info offset?
  DWORD offset;         /**< Address offset of proc or name index of
                             DLL if segment==0xFFFF. */
  WORD  segment;        ///< Segment.
  DWORD linkNameIdx;    ///< Index of linker name, if segment=0xFFFF?
};

/// @brief The payload of the Code View Data Reference Record
typedef CVsrProcRefPayload CVsrDataRefPayload;

struct TDSsrGlobalDataRefPayload {
  DWORD offSymRef;      /**< Always 0. */
  DWORD typeIdx;        ///< Type index. 0 if none.
  DWORD nameIdx;        ///< Index of name in sstNames
  DWORD brwsInfoOffset; ///< Browser info offset?
  DWORD offset;         /**< Address offset of proc or name index of
                             DLL if segment==0xFFFF. */
  WORD  segment;        ///< Segment.
};

/// @brief The header of the payload of the Code View Global Data Symbol Record
struct CVsrGlobalDataPayloadHdr {
   DWORD         offset;       ///< Address offset
   WORD          segment;      ///< Segment
   WORD          typeIdx;      ///< Index of type
};

/// @brief The payload of the TDS Global Data symbol record
struct TDSsrGlobalDataPayload {
   DWORD         offset;       ///< Address offset
   WORD          segment;      ///< Segment
   WORD          flags;        ///< 1=TLS var, 0=Not a TLS var.?
   DWORD         typeIdx;      ///< Index of type
   DWORD         nameIdx;      ///< Index of name in sstNames.
   DWORD         brwsInfoOffs; ///< Browser info offset?
};

/// @brief The header of the payload of the Code View BP Relative symbol record
struct CVsrBPRelativePayloadHdr {
   DWORD         offset;    ///< Signed offset relative to BP.
   WORD          typeIdx;   ///< Index of the type.
};

/// @brief The payload of the TDS BP Relative symbol record
struct TDSsrBPRelativePayload {
   DWORD         offset;    ///< Signed offset relative to BP.
   DWORD         typeIdx;   ///< Index of the type.
   DWORD         nameIdx;   ///< Index of the symbol name.
   DWORD         brwsOffs;  ///< Browser info offset.
};

/// @brief The header of the payload of the Code View Local Proc symbol record
struct CVsrLocalProcPayloadHdr {
   DWORD         parent;    ///< Parent of procedure relative to subsection.
   DWORD         end;       ///< End of procedure relative to subsection.
   DWORD         next;      ///< Next procedure relative to subsection.
   DWORD         procLen;   ///< Length of procedure in bytes.
   DWORD         dbgStart;  ///< Offset of code where stack frame is ok.
   DWORD         dbgEnd;    ///< Offset of code where return value is ok.
   DWORD         offset;    ///< Offset of 1st procedure code.
   WORD          segment;   ///< Segment of procedure code.
   WORD          typeIdx;   ///< Index of type (procedure signature)
   BYTE          flags;     ///< Procedure flags
};

/// @brief The payload of the TDS Local Proc symbol record.
///
/// The structure is appended with a length-prefixed linker name of the
/// procedure.
struct TDSsrLocalProcPayloadHdr {
   DWORD         parent;    ///< Parent of procedure relative to subsection.
   DWORD         end;       ///< End of procedure relative to subsection.
   DWORD         next;      ///< Next procedure relative to subsection.
   DWORD         procLen;   ///< Length of procedure in bytes.
   DWORD         dbgStart;  ///< Offset of code where stack frame is ok.
   DWORD         dbgEnd;    ///< Offset of code where return value is ok.
   DWORD         offset;    ///< Offset of 1st procedure code.
   WORD          segment;   ///< Segment of procedure code. ???
   WORD          flags;     ///< Not sure whether this is correct!
   DWORD         typeIdx;   ///< Index of type (procedure signature) ???
   DWORD         nameIdx;   ///< Index of procedure name.
   DWORD         brwsOffs;  ///< Browser info offset (not sure).
};

/// @brief The payload header of the DBG Block Start symbol record.
///
/// The structure is appended with a length-prefixed expression string.
struct CVsrBlock32PayloadHdr {
   DWORD         parent;    ///< Parent of block relative to subsection.
   DWORD         end;       ///< End of block relative to subsection.
   DWORD         next;      ///< Next block relative to subsection.
   DWORD         len;       ///< Length of block in bytes.
   DWORD         offset;    ///< Offset of block.
   WORD          segment;   ///< Segment of block.
};

/// @brief The payload of the TDS Block Start symbol record.
struct TDSsrBlock32Payload : public CVsrBlock32PayloadHdr {
   DWORD         nameIdx;   ///< Name index of expression string.
};

#ifdef __BORLANDC__
#pragma option pop
#endif

#endif

// Local Variables:
// mode:c++
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
