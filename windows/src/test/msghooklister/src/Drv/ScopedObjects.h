#ifndef SCOPED_OBJECTS_H
#define SCOPED_OBJECTS_H

#pragma once

#ifdef BUILDING_DRIVER
#pragma code_seg("PAGE")
#define DoPaged() PAGED_CODE()
#else
#define DoPaged() (void)0
#endif

#pragma warning(push)
// disable the conditional error is constant warnings
// caused by the ASSERTs
#pragma warning(disable : 4127)

template<class Type>
class ScopedArray
{
private:
	Type* data;

public:
	ScopedArray(Type* data) : data(data)
	{DoPaged();}

	ScopedArray() : data(NULL)
	{DoPaged();}

	~ScopedArray()
	{
		DoPaged();
		if(data)
		{
			delete [] data;
		}
	}

	Type* get() const
	{
		DoPaged();
		ASSERT(data && "Array not initialized");
		return data;
	}

	Type** operator&()
	{
		DoPaged();
		ASSERT(!data && "Array already initialized");
		return &data;
	}

	Type* operator*() const
	{
		DoPaged();
		return get();
	}

	void Init(Type* newData)
	{
		DoPaged();
		ASSERT(!data && "Array already initialized");
		data = newData;
	}
};

#ifdef BUILDING_DRIVER
#define STANDARD_RET_TYPE NTSTATUS
#else
#define STANDARD_RET_TYPE BOOL
#endif

template<class Type = HANDLE, class Ret = STANDARD_RET_TYPE>
class WinType
{
private:
	typedef Ret (NTAPI*ReleaseProc)(Type);
	Type data;
	ReleaseProc lpfnReleaser;

public:
	WinType(const Type& data, ReleaseProc lpfnReleaser)
		: data(data), lpfnReleaser(lpfnReleaser)
	{DoPaged();}

	WinType()
		: data(), lpfnReleaser(NULL)
	{DoPaged();}

	~WinType()
	{
		DoPaged();
		if(lpfnReleaser)
		{
			lpfnReleaser(data);
		}
	}

	Type get() const
	{
		DoPaged();
		return data;
	}

	Type operator*() const
	{
		DoPaged();
		return get();
	}

	Type* operator&()
	{
		DoPaged();
		ASSERT(!data && lpfnReleaser && "Object already initialized");
		return &data;
	}

	void Init(const Type& newData, ReleaseProc lpfnNewReleaser)
	{
		DoPaged();
		ASSERT(lpfnReleaser == NULL && "Object not initialized");
		data = newData;
		lpfnReleaser = lpfnNewReleaser;
	}

	Type Release()
	{
		DoPaged();
		lpfnReleaser = NULL;
		Type temp = data;
		data = Type();
		return temp;
	}
};

#ifdef BUILDING_DRIVER

struct FastMutexLock
{
private:
	PKGUARDED_MUTEX pMutexLock;

public:
	__drv_neverHoldCriticalRegion
	__drv_acquiresCriticalRegion
	inline FastMutexLock(PKGUARDED_MUTEX pMutex) : pMutexLock(pMutex)
	{
		DoPaged();
		KeAcquireGuardedMutex(pMutexLock);
	}

	__drv_mustHoldCriticalRegion
	__drv_releasesCriticalRegion
	inline ~FastMutexLock()
	{
		DoPaged();
		KeReleaseGuardedMutex(pMutexLock);
	}
};

struct ExclusiveResourceLock
{
private:
	PERESOURCE pEResource;

public:
	__drv_maxIRQL(APC_LEVEL)
	__drv_acquiresCriticalRegion
	inline ExclusiveResourceLock(PERESOURCE pEResource)
		: pEResource(pEResource)
	{
		DoPaged();
		ExEnterCriticalRegionAndAcquireResourceExclusive(pEResource);
	}

	__drv_maxIRQL(DISPATCH_LEVEL)
	__drv_releasesCriticalRegion
	inline ~ExclusiveResourceLock()
	{
		DoPaged();
		ExReleaseResourceAndLeaveCriticalRegion(pEResource);
	}
};

#endif

#ifdef BUILDING_DRIVER
#pragma code_seg()
#endif

#pragma warning(pop)

#endif
