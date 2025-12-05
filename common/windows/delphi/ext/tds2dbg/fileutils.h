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

/// @file fileutils.h File utilities

#ifndef FILEUTILS_H
#define FILEUTILS_H

#include <stdio.h>
#include <vector>

/// @brief Base class for file access wrappers.
class FileWrapper {
public:

   /// @brief Constructor.
   ///
   /// @param f  The file stream being wrapped. Ownership not transferred.
   FileWrapper(FILE* f);

   /// @brief Returns the current read/write position.
   /// @return The current write position.
   unsigned int tell() const;

   /// @brief Sets the current read/write position.
   ///
   /// @param pos The new (absolute) write position.
   ///
   /// @throw runtime_error If an error occured.
   void seek(unsigned int pos) throw(std::runtime_error);

   /// @brief Skips a number of bytes from the input stream.
   ///
   /// @param amount The number of bytes to skip.
   ///
   /// @throw runtime_error If an error occured.
   void skip(unsigned int amount) throw(std::runtime_error);

   /// @brief Reads a sequence of structures from the current read
   ///        position and throws an exception if end of file is
   ///        reached prematurely.
   ///
   /// @param dst  The 1st structure to hold the data read in.
   /// @param num  The number of structures to read.
   ///
   /// @tparam The class of the structures to read.
   ///
   /// @throw runtime_error If not all bytes could be read.
   template <class T>
   void read(T& dst, unsigned int num = 1) throw(std::runtime_error) {
      unsigned int n = fread(&dst,1,num*sizeof(T),m_f);
      if(num*sizeof(T) != n) {
         throw std::runtime_error("Unexpected end of file");
      }
   }

   /// @brief Attempts to reads a sequence of structures from the
   ///        current read position.
   ///
   /// @param dst  The 1st structure to hold the data read in.
   /// @param num  The number of structures to read.
   ///
   /// @return The number of bytes actually read.
   ///
   /// @tparam The class of the structures to read.
   template <class T>
   unsigned int tryRead(T& dst, unsigned int num = 1) {
      return fread(&dst,1,num*sizeof(T),m_f);
   }

   /// @brief Writes a sequence of bytes to the current write position.
   ///
   /// @param src Reference to the 1st structure to be written.
   /// @param num The number of instances to write.
   ///
   /// @return The number of bytes actually written.
   ///
   /// @tparam T Class of the structure to write.
   ///
   /// @throw runtime_error If an error occured.
   template <class T>
   unsigned int
   write(const T& src, unsigned int num = 1) throw(std::runtime_error) {
      unsigned int n = fwrite(&src,1,num*sizeof(T),m_f);
      if(n != num*sizeof(T)) {
         throw std::runtime_error("Write error in write()");
      }
      return n;
   }

   /// @brief Writes multiple copies of the same structure starting from the
   ///        current write position.
   ///
   /// @param src Reference to the structure to be written.
   /// @param num The number of copies to write.
   ///
   /// @return The number of bytes actually written.
   ///
   /// @tparam T Class of the structure to write.
   ///
   /// @throw runtime_error If an error occured.
   template <class T>
   unsigned int
   writeRep(const T& src, unsigned int num = 1) throw(std::runtime_error);
   
   /// @brief Writes a (1-byte) length-prefixed name to the current
   ///        write position.
   ///
   /// @param name  The name to write.
   ///
   /// @throw runtime_error If an error occured.
   void write1bLpfName(const std::string& name) throw(std::runtime_error);

protected:

   FILE* m_f; ///< The file stream.

};


template <class T>
unsigned int
FileWrapper::writeRep(const T& src, unsigned int num)
   throw(std::runtime_error)
{
   unsigned int n = 0;
   while(num--) {
      unsigned int m = fwrite(&src,1,sizeof(T),m_f);
      if(sizeof(T) != m) {
         throw std::runtime_error("Write error in writeRep()");
      }
      n += m;
   }
   return n;
}

/// @brief Reads a record sequentially from a file while keeping track
///        of the number of bytes available.
class RecordReader {
public:

   /// @brief Constructor.
   ///
   /// @param tdsFile The file to read from.
   /// @param recSz   The number of bytes in the record.
   RecordReader(FileWrapper& tdsFile, unsigned int recSz);

   /// @brief Returns the current read position.
   ///
   /// @return The current (absolute) read position.
   unsigned int tell() {
      return m_tdsFile.tell();
   }

   /// @brief Determines whether an end-of-record condition is met.
   ///
   /// @retval true  if all bytes have been read.
   /// @retval false otherwise.
   bool eor() const {
      return (m_tdsFile.tell() >= m_endPos);
   }

   /// @brief Sets the current absolute read position.
   ///
   /// @param pos The new (absolute) read position.
   ///
   /// @throw runtime_error In case of errors.
   void seek(unsigned int pos) throw(std::runtime_error) {
      m_tdsFile.seek(pos);
   }

   /// @brief Skips a number of bytes from the input stream.
   ///
   /// @param amount The number of bytes to skip.
   ///
   /// @throw runtime_error In case of errors.
   void skip(unsigned int amount) throw(std::runtime_error) {
      m_tdsFile.skip(amount);
   }

   /// @brief Reads a sequence of structures from the current read
   ///        position in the TDS file.
   ///
   /// @param dst  The 1st structure to hold the data read in.
   /// @param num  The number of structures to read.
   ///
   /// @throw runtime_error In case of errors.
   template <class T>
   void read(T& dst, unsigned int num = 1) throw(std::runtime_error) {
      m_tdsFile.read(dst,num);
   }

protected: 

   FileWrapper& m_tdsFile; ///< The file to read from.

   unsigned int m_recPos;  ///< The position of the current record.

   unsigned int m_endPos;  ///< The position after the end of the record list.

};

/// @brief Registers data for later writing to a file while counting
///        the number of bytes to be written.
class RecordWriter {
public:

   /// @brief Destructor.
   ~RecordWriter();

   /// @brief Commits the data registered to a file at the current write
   ///        position of the file.
   ///
   /// @param file The file to write to.
   ///
   /// @throw runtime_error In case of errors.
   void commit(FileWrapper& f) const throw(std::runtime_error);

   /// @brief Returns the number of bytes registered for later writing.
   ///
   /// @return The number of bytes registered.
   unsigned int length() const;

   /// @brief Appends a sequence of structures to the tail of the
   ///        write queue.
   ///
   /// The data provided is copied into the internal write queue.
   ///
   /// @param dst  The 1st structure that holds the data to be written.
   /// @param num  The number of structures to write.
   ///
   /// @return The number of bytes written.
   template <class T>
   unsigned int write(const T& src, unsigned int num = 1) {
      unsigned int       n = sizeof(T)*num;
      WriteQueueElement* e;

      if(0 == m_writeQ.size()) {
         e = new CopiedData();
         m_writeQ.push_back(e);
      }

      e = m_writeQ.back()->append(reinterpret_cast<const unsigned char*>(&src),n);
      if(e) {
         // New element created, append it to the queue.
         m_writeQ.push_back(e);
      }
      return n;
   }

   /// @brief Writes multiple copies of the same structure starting from the
   ///        current write position.
   ///
   /// @param src Reference to the structure to be written.
   /// @param num The number of copies to write.
   ///
   /// @return The number of bytes actually written.
   ///
   /// @tparam T Class of the structure to write.
   template <class T>
   unsigned int writeRep(const T& src, unsigned int num = 1) {
      unsigned int n = 0;
      while(num--) {
         n+= this->write(src);
      }
      return n;
   }

   /// @brief Appends a sequence of structures to the tail of the
   ///        write queue.
   ///
   /// This menthod appends a reference to the data to the write
   /// queue.  The data provided is not copied.
   ///
   /// @param dst  The 1st structure that holds the data to be written.
   /// @param num  The number of structures to write.
   ///
   /// @return The number of bytes written.
   template <class T>
   unsigned int delayedWrite(const T& src, unsigned int num = 1) {
      unsigned int n = sizeof(T)*num;
      m_writeQ.
         push_back(new
                   ReferencedData(reinterpret_cast<unsigned char*>(&src),n));
      return n;
   }

   /// @brief Appends a (1-byte) length-prefixed name to the write queue.
   ///
   /// @param name  The name to write.
   void write1bLpfName(const std::string& name);

protected:

   /// @brief Base class for write queue data.
   class WriteQueueElement
   {
   public:

      /// @brief Destructor.
      virtual ~WriteQueueElement();

      /// @brief Returns the number of bytes registered within this element.
      ///
      /// @return The number of bytes registered within this element.
      virtual unsigned int length() const = 0;

      /// @brief Commits the data in this element to the current write
      ///        position of a file.
      ///
      /// @param f The file to write to.
      ///
      /// @throw runtime_error In case of errors.
      virtual void commit(FileWrapper& f) const throw(std::runtime_error) = 0;

      /// @brief Appends a copy of a sequence of bytes to the current
      ///        element.
      ///
      /// @param src  The first byte to copy.
      /// @param len  The number of bytes to copy.
      ///
      /// @return 0 if the bytes were appended to the current element, or
      ///         a pointer to a new element that received the copied bytes.
      virtual
      WriteQueueElement* append(const unsigned char* src, unsigned int len) = 0;

   };

   /// @brief Data copied into the write queue.
   class CopiedData : public WriteQueueElement
   {
   public:

      // Overridden base class method.
      unsigned int length() const;

      // Overridden base class method.
      void commit(FileWrapper& f) const throw(std::runtime_error);

      // Overridden base class method.
      WriteQueueElement* append(const unsigned char* src, unsigned int len);

   protected:

      std::vector<unsigned char> m_data;

   };

   /// @brief Data referenced from the write queue.
   class ReferencedData : public WriteQueueElement
   {
   public:

      // Overridden base class method.
      unsigned int length() const;

      // Overridden base class method.
      void commit(FileWrapper& f) const throw(std::runtime_error);

      // Overridden base class method.
      WriteQueueElement* append(const unsigned char* src, unsigned int len);

      /// @brief Constructor.
      ///
      /// @param src  The first byte to reference.
      /// @param len  The number of subsequent bytes to reference.
      ReferencedData(const unsigned char* src, unsigned int len);

   protected:

      const unsigned char* m_src; ///< The first byte referenced.
      unsigned int         m_len; ///< The number of bytes referenced.

   };

protected:

   /// @brief Container underlying the internal write queue.
   typedef std::vector<WriteQueueElement*> WriteQCont;

   /// @brief The internal write queue.
   WriteQCont m_writeQ;

};

/// @brief Postpones writing of a record until committed.
///
/// @note It is important to create an instance of this class before writing
///       any records that should follow it (in file order).
///
/// @tparam R The record structure.
template <class R>
class PpRecordWriter {
public:

   /// @brief Constructor.
   ///
   /// @param dbgFile The DBG file to write to.
   /// @param rec     The record to be written when committed to.
   PpRecordWriter(FileWrapper& dbgFile, R& rec) :
      m_dbgFile(dbgFile), m_rec(rec), m_len(0)
   {
      m_recPos = dbgFile.tell();
      dbgFile.skip(sizeof(R));
   }

   /// @brief Commits to the record.
   ///
   /// This method will write out the record.
   ///
   /// @throw runtime_error In case of errors.
   void commit() throw(std::runtime_error) {
      unsigned int oldPos = m_dbgFile.tell();
      m_dbgFile.seek(m_recPos);
      m_len = m_dbgFile.write(m_rec);
      m_dbgFile.seek(oldPos);
   }

   /// @brief Returns the number of bytes written.
   /// @return The number of bytes written.
   unsigned int length() const {
      return m_len;
   }

protected:

   /// @brief The DBG file to write to.
   FileWrapper& m_dbgFile;

   /// @breif The record to write.
   R&           m_rec;

   /// @brief File position of the record.
   unsigned int m_recPos;

   /// @brief Number of bytes written.
   unsigned int m_len;

};

/// @brief Postpones writing of a header with a length field to be
///        computed until committed.
///
/// @tparam H  The header structure.
/// @tparam L  The type of the length field.
template <class H, class L>
class HeaderWithLen {
public:

   /// @brief Constructor.
   ///
   /// @param hdr     The header to be written when committed to.
   ///                Must remain available during the entire lifetime of the
   ///                object created.
   /// @param lenFld  The length field to update. Must remaining available
   ///                during the entire lifetime of the object created.
   HeaderWithLen(H& hdr, L& lenFld) : m_hdr(hdr), m_lenFld(lenFld)
   {
   }

   /// @brief Writes the header and the data registered by a RecordWriter.
   ///
   /// This method will write out the header with an updated length field
   /// as well as the data registered by the RecordWriter.
   /// The length field will be computed as the size of the header, plus the
   /// size of the data written by the RecordWriter, minus the size of the
   /// length field itself.
   ///
   /// @param f The file to write to.
   /// @param w The record writer that was used to write the data committed to.
   ///
   /// @throw runtime_error In case of errors.
   void
   commit(FileWrapper& f, const RecordWriter& w) throw(std::runtime_error){
      m_lenFld = sizeof(H) + w.length()-sizeof(L);
      f.write(m_hdr);
      w.commit(f);
   }

   /// @brief Returns the number of bytes (to be) written by the record writer
   ///        and the header writer itself.
   ///
   /// @return The number of bytes (to be) written.
   unsigned int length(const RecordWriter& w) const {
      return sizeof(H) + w.length();
   }

protected:

   /// @brief The header to write.
   H&           m_hdr;

   /// @brief The length field to update.
   L&           m_lenFld;

};

/// @brief Writes a sequence of bytes from one file object to another.
///
/// @param dst    Destination file object.
/// @param src    Source file object.
/// @param amount Number of bytes to copy.
///
/// @tparam D Class of the destination object.
/// @tparam S Class of the source object.
///
/// @throw runtime_error In case of errors.
template <class D, class S>
void
copyFileContent(D& dst, S& src, unsigned int amount = 1)
   throw(std::runtime_error)
{
   unsigned char buf[512];
   unsigned int  toRead = (sizeof buf);
   while(amount) {
      if(toRead>amount) {
         toRead = amount;
      }
      src.read(buf[0],toRead);
      amount -= toRead;
      dst.write(buf[0],toRead);
   }
}

/// @brief Appends the contents of one file to another.
///
/// @param dst    Destination file object.
/// @param src    Source file object.
///
/// @tparam D Class of the destination object.
/// @tparam S Class of the source object.
///
/// @throw runtime_error In case of errors.
template <class D, class S>
void
copyFile(D& dst, S& src) throw(std::runtime_error)
{
   unsigned char buf[512];
   unsigned int  bytesRead;
   do {
      bytesRead = src.tryRead(buf[0],(sizeof buf));
      dst.write(buf[0],bytesRead);
   } while((sizeof buf) == bytesRead);
}

#endif

// Local Variables:
// mode:c++
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
