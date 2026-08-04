
#ifndef __CRC32_H
#define __CRC32_H

unsigned long CalculateBufferCRC(unsigned char *p, unsigned long count);
unsigned long CalculateBufferCRC(unsigned long count, unsigned char *p);
void BuildCRCTable();
void Dehash(unsigned char *buf, unsigned long len);

#endif
