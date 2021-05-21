#ifndef SIMPLE_TRAITS_H
#define SIMPLE_TRAITS_H

#pragma once

// some simple type traits impls, so we don't need boost or TR1
template<class A, class B>
struct is_same
{
	static const bool value = false;
};

template<class A>
struct is_same<A, A>
{
	static const bool value = true;
};

template<class A>
struct remove_pointer
{
	typedef A type;
};

template<class A>
struct remove_pointer<A*>
{
	typedef A type;
};

#endif
