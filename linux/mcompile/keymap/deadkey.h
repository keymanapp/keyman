
#pragma once
#ifndef DEADKEY_H
#define DEADKEY_H

#include <map>

v_dw_1D createLine(std::wstring  first, std::wstring second,  KMX_DWORD number, std::wstring nameresult);
KMX_DWORD create_DKTable(v_dw_2D & dk_ComposeTable);

KMX_DWORD find_dkCharacter(v_dw_2D * dk_ComposeTable, KMX_DWORD first, KMX_DWORD second );

void find_all_dk_combinations(v_dw_2D * dk_ComposeTable, v_dw_2D & dk_CombinationTable, KMX_DWORD dk);
KMX_DWORD getKeyname(KMX_DWORD in, KMX_DWORD &shift) ;

# endif /*DEADKEY_H*/