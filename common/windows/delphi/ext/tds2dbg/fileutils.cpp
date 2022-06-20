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

/// @file fileutils.cpp File utilities

#include "fileutils.h"

FileWrapper::FileWrapper(FILE* f) : m_f(f)
{
}

unsigned int
FileWrapper::tell() const {
   return ftell(m_f);
}

void
FileWrapper::seek(unsigned int pos) throw(std::runtime_error) {
   if(fseek(m_f,pos,SEEK_SET)) {
      throw std::runtime_error("Failed to set read/write position");
   }
}

void
FileWrapper::skip(unsigned int amount) throw(std::runtime_error) {
   if(fseek(m_f,amount,SEEK_CUR)) {
      throw std::runtime_error("Failed to advance read/write position");
   }
}

void
FileWrapper::write1bLpfName(const std::string& name) throw(std::runtime_error)
{
   unsigned char len = (unsigned char) (name.length());

   if((sizeof len) != fwrite(&len,1,(sizeof len),m_f)) {
      throw std::runtime_error("Failed to write length field of string");
   }
   if((unsigned int)(len+1) != fwrite(name.c_str(),1,len+1,m_f)) {
      throw std::runtime_error("Failed to write string contents");
   }
}

RecordReader::RecordReader(FileWrapper& tdsFile, unsigned int recSz) :
   m_tdsFile(tdsFile)
{
   m_recPos = tdsFile.tell();
   m_endPos = m_recPos + recSz;
}

RecordWriter::~RecordWriter()
{
   WriteQCont::iterator it = m_writeQ.begin();
   while(m_writeQ.end() != it) {
      delete *it++;
   }
}

void
RecordWriter::commit(FileWrapper& f) const throw(std::runtime_error)
{
   WriteQCont::const_iterator it = m_writeQ.begin();
   while(m_writeQ.end() != it) {
      (*it++)->commit(f);
   }
}

unsigned int
RecordWriter::length() const
{
   unsigned int               len = 0;
   WriteQCont::const_iterator it  = m_writeQ.begin();
   while(m_writeQ.end() != it) {
      len += (*it++)->length();
   }
   return len;
}

void
RecordWriter::write1bLpfName(const std::string& name)
{
   unsigned char len = (unsigned char) (name.length());

   this->write(len);
   this->write(*(name.c_str()),len+1);
}

RecordWriter::WriteQueueElement::~WriteQueueElement()
{
}

unsigned int
RecordWriter::CopiedData::length() const
{
   return m_data.size();
}

void
RecordWriter::CopiedData::commit(FileWrapper& f) const throw(std::runtime_error)
{
   f.write(m_data.front(),m_data.size());
}

RecordWriter::WriteQueueElement*
RecordWriter::CopiedData::append(const unsigned char* src, unsigned int len)
{
   m_data.reserve(m_data.size()+len);
   while(len--) {
      m_data.push_back(*src++);
   }
   return 0;
}

RecordWriter::ReferencedData::
ReferencedData(const unsigned char* src, unsigned int len)
   : m_src(src), m_len(len)
{
}

unsigned int
RecordWriter::ReferencedData::length() const
{
   return m_len;
}

void
RecordWriter::ReferencedData::commit(FileWrapper& f) const
   throw(std::runtime_error)
{
   f.write(*m_src,m_len);
}

RecordWriter::WriteQueueElement*
RecordWriter::ReferencedData::append(const unsigned char* src, unsigned int len)
{
   CopiedData* c = new CopiedData();
   c->append(src,len);
   return c;
}


// Local Variables:
// mode:c++
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
