{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazideminimap;

{$warn 5023 off : no warning about unused units}
interface

uses
  frmlazideminimap, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('frmlazideminimap', @frmlazideminimap.Register);
end;

initialization
  RegisterPackage('lazideminimap', @Register);
end.
