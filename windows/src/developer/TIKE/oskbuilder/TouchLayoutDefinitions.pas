(*
  Name:             TouchLayoutDefinitions
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      21 Feb 2014

  Modified Date:    21 Feb 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          21 Feb 2014 - mcdurdin - I4062 - V9.0 - Touch Layout platforms do not allow font size specification

*)
unit TouchLayoutDefinitions;

interface

uses
  System.JSON;

type
  TJSONClass = class of TJSONAncestor;
  PJSONDef = ^TJSONDef;
  TJSONDef = record
    Name: string;
    ClassType: TJSONClass;
    Required: Boolean;
    Value: PJSONDef;
    ValueSize: Integer;
  end;
  TJSONDefArray = array of TJSONDef;

const
  SkDef: array[0..9] of TJSONDef = (
    (Name: 'id'; ClassType: TJSONString; Required: True),
    (Name: 'text'; ClassType: TJSONString),
    (Name: 'sp'; ClassType: TJSONValue),
    (Name: 'width'; ClassType: TJSONValue),
    (Name: 'pad'; ClassType: TJSONValue),
    (Name: 'dk'; ClassType: TJSONValue),
    (Name: 'layer'; ClassType: TJSONString),
    (Name: 'nextlayer'; ClassType: TJSONString),
    (Name: 'font'; ClassType: TJSONString),
    (Name: 'fontsize'; ClassType: TJSONString)
  );

  KeyDef: array[0..10] of TJSONDef = (
    (Name: 'id'; ClassType: TJSONString),
    (Name: 'text'; ClassType: TJSONString),
    (Name: 'sp'; ClassType: TJSONValue),
    (Name: 'width'; ClassType: TJSONValue),
    (Name: 'pad'; ClassType: TJSONValue),
    (Name: 'dk'; ClassType: TJSONValue),
    (Name: 'layer'; ClassType: TJSONString),
    (Name: 'nextlayer'; ClassType: TJSONString),
    (Name: 'font'; ClassType: TJSONString),
    (Name: 'fontsize'; ClassType: TJSONString),
    (Name: 'sk'; ClassType: TJSONArray; Value: @SkDef[0]; ValueSize: Length(SkDef))
  );

  RowDef: array[0..1] of TJSONDef = (
    (Name: 'id'; ClassType: TJSONString; Required: True),
    (Name: 'key'; ClassType: TJSONArray; Required: True; Value: @KeyDef[0]; ValueSize: Length(KeyDef))
  );

  LayerDef: array[0..1] of TJSONDef = (
    (Name: 'id'; ClassType: TJSONString; Required: True),
    (Name: 'row'; ClassType: TJSONArray; Required: True; Value: @RowDef[0]; ValueSize: Length(RowDef))
  );

  PlatformDef: array[0..3] of TJSONDef = (
    (Name: 'font'; ClassType: TJSONString),
    (Name: 'fontsize'; ClassType: TJSONString),   // I4062
    (Name: 'displayUnderlying'; ClassType: TJSONBool),
    (Name: 'layer'; ClassType: TJSONArray; Required: True; Value: @LayerDef[0]; ValueSize: Length(LayerDef))
  );

  LayoutDef: array[0..2] of TJSONDef = (
    (Name: 'phone'; ClassType: TJSONObject; Value: @PlatformDef[0]; ValueSize: Length(PlatformDef)),
    (Name: 'tablet'; ClassType: TJSONObject; Value: @PlatformDef[0]; ValueSize: Length(PlatformDef)),
    (Name: 'desktop'; ClassType: TJSONObject; Value: @PlatformDef[0]; ValueSize: Length(PlatformDef))
  );

const
  CTouchPlatforms: array[0..2] of string = ('tablet', 'desktop', 'phone');

type
  TTouchLayoutPredefinedLayer = string;

const
  PredefinedLayers: array[0..23] of TTouchLayoutPredefinedLayer = (
    'default',

    // Legacy predefined layer names
    'shift',
    'ctrl',
    'alt',
    'ctrlalt',
    'ctrlshift',
    'altshift',
    'ctrlaltshift',

    // 10.0 predefined layer names
    //'default',
    //'shift',
    //'ctrl',
    'shift-ctrl',
    //'alt',
    'shift-alt',
    'ctrl-alt',
    'shift-ctrl-alt',

    //'default',
    'leftctrl',
    'rightctrl',
    'leftalt',
    'leftctrl-leftalt',
    'rightalt',
    'rightctrl-rightalt',
    //'shift',
    'leftctrl-shift',
    'rightctrl-shift',
    'leftalt-shift',
    'rightalt-shift',
    'leftctrl-leftalt-shift',
    'rightctrl-rightalt-shift'
  );

implementation

end.
