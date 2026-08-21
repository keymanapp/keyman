    {$ifdef VER340}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    // Keyman local patch: Delphi 11/12 compat (vendored mbcolor). Without
    // these blocks DELPHI_*_UP flags are never defined on VER350/VER360,
    // which causes HTMLColors.pas to drop "uses Variants" and fail to
    // resolve the Null constant. On mbcolor refresh: re-apply if needed.
    {$ifdef VER350}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    {$ifdef VER360}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    {$ifdef VER330}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    {$ifdef VER320}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    {$ifdef VER310}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    {$ifdef VER230}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    {$ifdef VER180}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    {$ifdef VER170}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
    {$endif}

    {$ifdef VER160}        
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
    {$endif}

    {$ifdef VER150}        
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
    {$endif}

    {$ifdef VER140}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
    {$endif}

    {$ifdef VER130}
     {$define DELPHI_5_UP}
    {$endif}

    {.$DEFINE mbXP_Lib}