

class CryptRC4
{
private:
	BOOL fInitialized;
	BYTE KeyData[256], KeyOrg[256];
	void EncryptBuffer(PBYTE InData, PBYTE OutData, DWORD Size);
	void DecryptBuffer(PBYTE InData, PBYTE OutData, DWORD Size);
public:
	static BOOL SelfTest();
	void Init(PBYTE Key, DWORD Size, PVOID InitVector);
	void Burn();
	void Reset();
	void Encrypt(PBYTE InData, PBYTE OutData, DWORD Size);
	void Decrypt(PBYTE InData, PBYTE OutData, DWORD Size);
};
