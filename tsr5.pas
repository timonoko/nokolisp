Program TSR;


{$M $1000,0,0}

uses dos;

var command:char;
  code:integer;
  a0,a1,a2,a3:integer;
  cD,c0,c1,c2,c3,para1,para2: string[127];
  o1,o2,o3:string[127];
  r1,r2,r3:real;i3:longint;
  OldInt05h:procedure;
  f:text;

  {$f+}
Procedure NewInt05h; interrupt;
begin
   val(o1,r1,code);
   val(o2,r2,code);
   case command of
    '+': r3:=(r1+r2);
    '-': r3:=(r1-r2);
    '*': r3:=(r1*r2);
    '/': if (r2<1.0E-30) then r3:=1.0E36 else r3:=(r1/r2);
    'i': if r1<1000000000 then r3:=trunc(r1) else r3:=r1;
    's': r3:=sin(r1);
    'a': r3:=abs(r1);
    'r': r3:=arctan(r1);
    'q': r3:=sqrt(r1);
    'l': if (r1<r2) then r3:=1 else r3:=0;
    'g': if (r1>r2) then r3:=1 else r3:=0;
    'e': if (r1=r2) then r3:=1 else r3:=0;
   end;
  if (abs(r3)<1000000000) and  (r3=trunc(r3)) then
    begin i3:=trunc(r3); str(i3,o3) end
    else str(r3,o3);
end;
{$f-}

begin
 para1:='\sys\noko.exe';
 if ParamCount>0 then para1:=ParamStr(1);
 para2:='\sys\tsr5.lsp';
 if ParamCount>1 then para2:=ParamStr(2);
 GetIntVec(250,@OldInt05h);
 SetIntVec(250,@NewInt05h);
       str(DSEG,cD);
       a0:=integer(addr(command));
       str(a0,c0);
       a1:=integer(addr(o1));
       str(a1,c1);
       a2:=integer(addr(o2));
       str(a2,c2);
       a3:=integer(addr(o3));
       str(a3,c3);
       assign(f,'jatkoa.bat');
       rewrite(f);
      writeln(f,para1,' (load (quote '+para2+')t) '+cd+' '
      				+c0+' '+c1+' '+c2+' '+c3+')');
      close(f);
      keep(0);

{ Heti-exec-versio:
       SwapVectors;
      Exec(para1,'((load (quote '+para2+')) '+cd+' '+c0+' '
          +c1+' '+c2+' '+c3+')');
      SwapVectors;
      if DosError <> 0 then
        WriteLn('Could not execute '+para1);
 SetIntVec(250,@OldInt05h);
}

end.
