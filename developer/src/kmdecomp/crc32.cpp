#include "pch.h"

/*
 * Instead of performing a straightforward calculation of the 32 bit
 * CRC using a series of logical operations, this program uses the
 * faster table lookup method.  This routine is called once when the
 * program starts up to build the table which will be used later
 * when calculating the CRC values.
 */

#define CRC32_POLYNOMIAL     0xEDB88320L

unsigned long CRCTable[256];

static void BuildCRCTable(void)
{
	static BOOL TableBuilt = FALSE;
    int i;
    int j;
    unsigned long crc;

	if(!TableBuilt)
	{
		for(i = 0; i <= 255; i++)
		{
			crc = i;
			
			for(j = 8; j > 0; j--)
				if(crc & 1) crc = (crc >> 1) ^ CRC32_POLYNOMIAL; else crc >>= 1;
        
			CRCTable[i] = crc;
		}
	}
}


/*
 * This routine calculates the CRC for a block of data using the
 * table lookup method. It accepts an original value for the crc,
 * and returns the updated value.
 */

static unsigned long CalculateBufferCRC(unsigned long count, BYTE *p)
{
    unsigned long temp1;
    unsigned long temp2;
	unsigned long crc = 0xFFFFFFFFL;

    while (count-- != 0)
	{
        temp1 = ( crc >> 8 ) & 0x00FFFFFFL;
        temp2 = CRCTable[((int) crc ^ *p++) & 0xff];
        crc = temp1 ^ temp2;
    }
    
	return crc;
}


BOOL VerifyChecksum(LPBYTE buf, LPDWORD CheckSum, DWORD sz)
{
	DWORD tempcs;

	tempcs = *CheckSum;
	*CheckSum = 0;

	BuildCRCTable();
	return tempcs == CalculateBufferCRC(sz, buf);
}
