
#include "pch.h"

#define CRC32_POLYNOMIAL 0xEDB88320

unsigned long CRCTable[256];
int  TableBuilt = 0;

void BuildCRCTable()
{
	int i, j;
	unsigned long crc;

	if(!TableBuilt)
		for(i = 0; i < 256; i++)
		{
			crc = i;

			for(j = 8; j >= 1; j--)
				if((crc & 1)) crc = (crc >> 1) ^ CRC32_POLYNOMIAL; else crc >>= 1;

			CRCTable[i] = crc;
		}
	TableBuilt = 1;
}

/*
 * This routine calculates the CRC for a block of data using the
 * table lookup method. It accepts an original value for the crc,
 * and returns the updated value.
 */

unsigned long CalculateBufferCRC(unsigned char *p, unsigned long count)
{
	unsigned long temp1, temp2, crc;

	crc = 0xFFFFFFFF;

	if(!TableBuilt) BuildCRCTable();

    while(count > 0)
	{
        temp1 = (crc >> 8) & 0x00FFFFFF;
        temp2 = CRCTable[((int)crc ^ (int)*p) & 0xFF];
        crc = temp1 ^ temp2;
        p++; count--;
    }

	return crc;
}

unsigned long CalculateBufferCRC(unsigned long count, unsigned char *p)
{
	return CalculateBufferCRC(p, count);
}

void Dehash(unsigned char *buf, unsigned long len)
{
	while(len > 0)
	{
		*buf = *buf ^ 0x6D;
		buf++; len--;
	}
}
