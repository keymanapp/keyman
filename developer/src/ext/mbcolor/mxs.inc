    {$ifdef VER340}
     {$define DELPHI_5_UP}
     {$define DELPHI_6_UP}
     {$define DELPHI_7_UP}
     {$define DELPHI_8_UP}
     {$define DELPHI_9_UP}
     {$define DELPHI_10_UP}
    {$endif}

    // Keyman patch (D11/12, vendored mbcolor): define DELPHI_*_UP on VER350/VER360
    // too, else HTMLColors.pas drops "uses Variants" and Null won't resolve.
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