/*
  Name:             rc4
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Nov 2009

  Modified Date:    11 Dec 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Nov 2009 - mcdurdin - I934 - Prep for x64 - Unreferenced parameters
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
*/

#include "pch.h"

KMX_BOOL CryptRC4::SelfTest()
{
	const KMX_BYTE Key1[5] = {0x61,0x8A,0x63,0xD2,0xFB};
	const KMX_BYTE InData1[5] = {0xDC,0xEE,0x4C,0xF9,0x2C};
	const KMX_BYTE OutData1[5] = {0xF1,0x38,0x29,0xC9,0xDE};

	CryptRC4 rc4;
	KMX_BYTE Data[5];

	rc4.Init((PBYTE)Key1, sizeof(Key1)*8, NULL);
	rc4.Encrypt((PBYTE)InData1,Data,sizeof(Data));
	if(memcmp(Data, OutData1, sizeof(Data)) != 0) return FALSE;
	rc4.Reset();
	rc4.Decrypt(Data,Data,sizeof(Data));
	if(memcmp(Data, InData1, sizeof(Data)) != 0) return FALSE;
	rc4.Burn();
	return TRUE;
}

void CryptRC4::Init(PBYTE Key, KMX_DWORD Size, PVOID InitVector)
{
  UNREFERENCED_PARAMETER(InitVector);
	KMX_DWORD i, j, t;
	KMX_BYTE xKey[256];

	if(fInitialized) Burn();

  //inherited Init(Key,Size,nil);
	Size /= 8;
	i = 0;
  
	while(i < 255)
	{
		KeyData[i] = (KMX_BYTE) i;     xKey[i] = Key[i % Size];
		KeyData[i+1] = (KMX_BYTE) i+1; xKey[i+1] = Key[(i+1) % Size];
		KeyData[i+2] = (KMX_BYTE) i+2; xKey[i+2] = Key[(i+2) % Size];
		KeyData[i+3] = (KMX_BYTE) i+3; xKey[i+3] = Key[(i+3) % Size];
		KeyData[i+4] = (KMX_BYTE) i+4; xKey[i+4] = Key[(i+4) % Size];
		KeyData[i+5] = (KMX_BYTE) i+5; xKey[i+5] = Key[(i+5) % Size];
		KeyData[i+6] = (KMX_BYTE) i+6; xKey[i+6] = Key[(i+6) % Size];
		KeyData[i+7] = (KMX_BYTE) i+7; xKey[i+7] = Key[(i+7) % Size];
		i += 8;
	}

	j = 0;
	i = 0;

	while(i < 255)
	{
		j = (j+KeyData[i]+xKey[i]) & 0xFF;
		t = KeyData[i];
		KeyData[i] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		j = (j+KeyData[i+1]+xKey[i+1]) & 0xFF;
		t = KeyData[i+1];
		KeyData[i+1] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		j = (j+KeyData[i+2]+xKey[i+2]) & 0xFF;
		t = KeyData[i+2];
		KeyData[i+2] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		j = (j+KeyData[i+3]+xKey[i+3]) & 0xFF;
		t = KeyData[i+3];
		KeyData[i+3] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		j = (j+KeyData[i+4]+xKey[i+4]) & 0xFF;
		t = KeyData[i+4];
		KeyData[i+4] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		j = (j+KeyData[i+5]+xKey[i+5]) & 0xFF;
		t = KeyData[i+5];
		KeyData[i+5] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		j = (j+KeyData[i+6]+xKey[i+6]) & 0xFF;
		t = KeyData[i+6];
		KeyData[i+6] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		j = (j+KeyData[i+7]+xKey[i+7]) & 0xFF;
		t = KeyData[i+7];
		KeyData[i+7] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		i += 8;
	}
	memcpy(KeyOrg, KeyData, sizeof(KeyOrg));

	fInitialized = TRUE;
}

void CryptRC4::Reset()
{
	memcpy(KeyData, KeyOrg, sizeof(KeyData));
}

void CryptRC4::Burn()
{
	memset(KeyOrg,0xFF,sizeof(KeyOrg));
	memset(KeyData,0xFF,sizeof(KeyData));
	// inherited Burn;
}

void CryptRC4::Encrypt(PBYTE InData, PBYTE OutData, KMX_DWORD Size)
{
	KMX_DWORD BufferSize=8192;
	KMX_DWORD i, Read;

	i = 0;
	while(i < Size/BufferSize)
	{
		Read=8192;
		EncryptBuffer(InData+i*BufferSize, OutData+i*BufferSize, Read);
		i++;
	}

	if(Size % BufferSize != 0)
	{
		Read=Size % BufferSize;
		EncryptBuffer(InData+i*BufferSize, OutData+i*BufferSize, Read);
	}
}

void CryptRC4::EncryptBuffer(PBYTE InData, PBYTE OutData, KMX_DWORD Size)
{
	KMX_DWORD i, j, t, k;

	if(!fInitialized) return; //raise EDCP_cipher.Create('Cipher not initialized');
	i = 0; j = 0;
	for(k = 0; k < Size; k++)
	{
		i = (i+1) & 0xFF;
		t = KeyData[i];
		j = (j+t) & 0xFF;
		KeyData[i] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		t = (t + KeyData[i]) & 0xFF;
		OutData[k] = InData[k] ^ KeyData[t];
	}
}

void CryptRC4::Decrypt(PBYTE InData, PBYTE OutData, KMX_DWORD Size)
{
	KMX_DWORD BufferSize=8192;
	KMX_DWORD i, Read;

	i = 0;
	while(i < Size/BufferSize)
	{
		Read=8192;
		DecryptBuffer(InData+i*BufferSize, OutData+i*BufferSize, Read);
		i++;
	}

	if(Size % BufferSize != 0)
	{
		Read=Size % BufferSize;
		DecryptBuffer(InData+i*BufferSize, OutData+i*BufferSize, Read);
	}
}

void CryptRC4::DecryptBuffer(PBYTE InData, PBYTE OutData, KMX_DWORD Size)
{
	KMX_DWORD i, j, t, k;

	if(!fInitialized) return; //raise EDCP_cipher.Create('Cipher not initialized');
	i = 0; j = 0;
	for(k = 0; k < Size; k++)
	{
		i = (i + 1) & 0xFF;
		t = KeyData[i];
		j = (j + t) & 0xFF;
		KeyData[i] = KeyData[j];
		KeyData[j] = (KMX_BYTE) t;
		t = (t + KeyData[i]) & 0xFF;
		OutData[k] = InData[k] ^ KeyData[t];
	}
}
