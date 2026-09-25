(*
  * Keyman is copyright (C) SIL Global. MIT License.
  *
  * Notes: Enable checking for metered connection and background data restrictions.
*)
unit Keyman.Configuration.Util.NetworkConnection;
(**
  *  Winapi.Networking.Connectivity uses TWinRTImportHelper this means
  *  ROInitialize does not need to be called when using this unit
  *  as it is already managed.
  *)
interface


type
  TNetworkConnection = class
    (**
     * Checks if the current internet connection is restricted, roaming, or over its
     * data limit.
     * This learn microsoft article shows how to combine network costs to determine
     * if the connection is metered.
     * https://learn.microsoft.com/en-us/uwp/api/windows.networking.connectivity.connectionprofile?view=winrt-28000
     *
     * @returns True if the connection is metered, False otherwise.
     *)
    class function IsMetered: Boolean; static;
    (**
     * Checks if background data usage is explicitly restricted by the current network profile.
     *
     * @returns True if background data usage is restricted, False otherwise.
     *)
    class function IsBackgroundDataRestricted: Boolean; static;
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
    class function IsBackgroundUpdateBlocked: Boolean; static;
  end;
implementation

uses
  Sentry.Client,
  System.SysUtils,
  Winapi.CommonTypes,
  Winapi.Networking.Connectivity;

class function TNetworkConnection.IsMetered: Boolean;
var
  Profile: IConnectionProfile;
  CostLevel: IConnectionCost;
begin
  Result := False;
  try
    // Get the profile currently providing internet access
    Profile := TNetworkInformation.GetInternetConnectionProfile;
    if Assigned(Profile) then
    begin
      CostLevel := Profile.GetConnectionCost;
      if CostLevel <> nil then
        Result := (CostLevel.NetworkCostType <> NetworkCostType.Unrestricted)
          or CostLevel.Roaming
          or CostLevel.OverDataLimit;
    end;
  except
    on E: Exception do
      // If the WinRT network APIs are unavailable or throw (e.g. unusual
      // Windows SKUs, containers, Network List Manager service stopped),
      // treat the connection as non-metered so updates are not blocked.
      begin
        Result := False;
        SentryHandleException(E);
      end;
  end;
end;

class function TNetworkConnection.IsBackgroundDataRestricted: Boolean;
var
  Profile: IConnectionProfile;
  CostLevel: IConnectionCost;
  DataRestriction: IConnectionCost2;
begin
  Result := False;
  try
    Profile := TNetworkInformation.GetInternetConnectionProfile;
    if Profile <> nil then
    begin
      CostLevel := Profile.GetConnectionCost;
      if (CostLevel <> nil) and Supports(CostLevel, IConnectionCost2, DataRestriction) then
        Result := DataRestriction.BackgroundDataUsageRestricted;
    end;
  except
    on E: Exception do
      // See IsMetered: default to not-restricted if the WinRT APIs throw.
      begin
        Result := False;
        SentryHandleException(E);
      end;
  end;
end;

// Currently this checks for metered connection OR background
// data usage restricted. If a configuration item is added that
// provides the option to download on metered connections then
// this should be updated to include that logic
class function TNetworkConnection.IsBackgroundUpdateBlocked: Boolean;
begin
  Result := IsMetered OR IsBackgroundDataRestricted;
end;

end.
