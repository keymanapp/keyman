(*
  * Keyman is copyright (C) SIL Global. MIT License.
  *
  * Notes: Enable checking for metered connection and background data restrictions.
*)
unit UtilNetworkConnection;

interface

(**
 * Checks if the current internet connection is restricted, roaming, or over its
 * data limit.
 * This learn microsoft article shows how to combine network costs to determine
 * if the connection is metered.
 * https://learn.microsoft.com/en-us/uwp/api/windows.networking.connectivity.connectionprofile?view=winrt-28000
 *
 * @returns True if the connection is metered, False otherwise.
 *)
function IsMetered: Boolean;

(**
 * Checks if background data usage is explicitly restricted by the current network profile.
 *
 * @returns True if background data usage is restricted, False otherwise.
 *)
function IsBackgroundDataRestricted: Boolean;

(**
 * Determines whether background updates are blocked.
 *
 * @returns True if background updates are blocked, False if allowed.
 *
 * Note: Currently this checks for metered connection OR background
        data usage restricted. If a configuration item is added that
        provides the option to download on metered connections then
        this should be updated to include that logic
 *)
function IsBackgroundUpdateBlocked: Boolean;

implementation

uses
  System.SysUtils,
  Winapi.CommonTypes,
  Winapi.WinRT,
  Winapi.Networking.Connectivity;

function IsMetered: Boolean;
var
  Profile: IConnectionProfile;
  CostLevel: IConnectionCost;
begin
  Result := False;
  // Get the profile currently providing internet access
  Profile := TNetworkInformation.GetInternetConnectionProfile;

  if Profile <> nil then
  begin
    CostLevel := Profile.GetConnectionCost;
    Result := (CostLevel.NetworkCostType <> NetworkCostType.Unrestricted)
      or CostLevel.Roaming
      or CostLevel.OverDataLimit;
  end;
end;

function IsBackgroundDataRestricted: Boolean;
var
  Profile: IConnectionProfile;
  CostLevel: IConnectionCost;
  DataRestriction: IConnectionCost2;
begin
  Result := False;
  Profile := TNetworkInformation.GetInternetConnectionProfile;
  if Profile <> nil then
  begin
    CostLevel := Profile.GetConnectionCost;
    if (CostLevel <> nil) and Supports(CostLevel, IConnectionCost2, DataRestriction) then
    begin
        Result := DataRestriction.BackgroundDataUsageRestricted;
        Exit;
    end;
  end;
end;

// Currently this checks for metered connection OR background
// data usage restricted. If a configuration item is added that
// provides the option to download on metered connections then
// this should be updated to include that logic
function IsBackgroundUpdateBlocked: Boolean;
begin
  Result := IsMetered OR IsBackgroundDataRestricted;
end;

end.
