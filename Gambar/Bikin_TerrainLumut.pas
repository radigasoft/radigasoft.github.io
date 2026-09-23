unit Bikin_TerrainLumut;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GR32, GR32_Image, GR32_Layers, Math, JPEG, StdCtrls;

procedure BikinTanahLumut(Image321: TImage32); overload;
procedure BikinTanahLumut(Image321: TImage32; AW, AH: Integer); overload;
procedure BikinTanahLumutMouseDown(Image321: TImage32; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
procedure BikinTanahLumutMouseMove(Image321: TImage32; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
procedure BikinTanahLumutMouseUp(Image321: TImage32; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
procedure BikinTanahLumutSimpan(Image321: TImage32);

implementation

type
  TFloatBuf = array of Single;
  TSingleArray = array[0..$0FFFFFFF] of Single;
  PSingleArray = ^TSingleArray;
  TCardinalArray = array[0..$0FFFFFFF] of Cardinal;
  PCardinalArray = ^TCardinalArray;

  TMossPass1Thread = class(TThread)
  private
    FYStart, FYEnd, FW, FSeed: Integer;
    FISO, FMossLevel: Single;
    PbufT, PmC, PmM, PmT, PmD: PSingleArray;
  protected
    procedure Execute; override;
  public
    constructor Create(YStart, YEnd, W, Seed: Integer; ISO, MossLevel: Single;
      ABufT, AMC, AMM, AMT, AMD: Pointer);
  end;

  TMossPass3Thread = class(TThread)
  private
    FYStart, FYEnd, FW, FSeed: Integer;
    FISO, FMossLevel, FOpRetak, FOpMuda, FOpTerang, FOpTua: Single;
    PbufT, PmC, PmM, PmT, PmD: PSingleArray;
    PBits: PCardinalArray;
  protected
    procedure Execute; override;
  public
    constructor Create(YStart, YEnd, W, Seed: Integer; ISO, MossLevel,
      OpRetak, OpMuda, OpTerang, OpTua: Single;
      ABufT, AMC, AMM, AMT, AMD, ABits: Pointer);
  end;

var
  MossRowNext: Integer;
  JobCounter, JobTotal: Integer;
  JbA, JbT: PSingleArray;
  JbW, JbH, JbRad: Integer;
  JbInv: Double;
  JdSeed, JdW: Integer;
  JdDirt: PSingleArray;
  JdBits: PCardinalArray;

type
  TRowJobProc = procedure(y: Integer);

  TRowJobThread = class(TThread)
  private
    FProc: TRowJobProc;
  protected
    procedure Execute; override;
  public
    constructor Create(AProc: TRowJobProc);
  end;

function CoreCount: Integer;
var
  si: TSystemInfo;
begin
  GetSystemInfo(si);
  Result := si.dwNumberOfProcessors;
  if Result < 1 then
    Result := 1;
  if Result > 64 then
    Result := 64;
end;

constructor TRowJobThread.Create(AProc: TRowJobProc);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FProc := AProc;
end;

procedure TRowJobThread.Execute;
var
  y: Integer;
begin
  while True do
  begin
    y := InterlockedIncrement(JobCounter) - 1;
    if y >= JobTotal then
      Break;
    FProc(y);
  end;
end;

procedure RunRowJob(Proc: TRowJobProc; Total: Integer);
var
  n, i: Integer;
  ths: array of TRowJobThread;
  hs: array of THandle;
begin
  if Total < 1 then
    Exit;
  n := CoreCount;
  if n > Total then
    n := Total;
  JobCounter := 0;
  JobTotal := Total;
  SetLength(ths, n);
  SetLength(hs, n);
  for i := 0 to n - 1 do
  begin
    ths[i] := TRowJobThread.Create(Proc);
    hs[i] := ths[i].Handle;
  end;
  for i := 0 to n - 1 do
    ths[i].Resume;
  WaitForMultipleObjects(n, @hs[0], True, INFINITE);
  for i := 0 to n - 1 do
    ths[i].Free;
end;

function Hash2(ix, iy, seed: Integer): Single;
var
  h: Int64;
begin
  h := ((Int64(ix) and $FFFFFFFF) * 374761393) and $FFFFFFFF;
  h := (h + ((Int64(iy) and $FFFFFFFF) * 668265263)) and $FFFFFFFF;
  h := (h + ((Int64(seed) and $FFFFFFFF) * 1597334677)) and $FFFFFFFF;
  h := ((h xor (h shr 13)) * 1274126177) and $FFFFFFFF;
  h := h xor (h shr 16);
  Result := (h and $FFFFFF) / 16777216;
end;

function VNoise(x, y: Single; seed: Integer): Single;
var
  ix, iy: Integer;
  fx, fy, a, b, c, d: Single;
begin
  ix := Floor(x);
  iy := Floor(y);
  fx := x - ix;
  fy := y - iy;
  fx := fx * fx * (3 - 2 * fx);
  fy := fy * fy * (3 - 2 * fy);
  a := Hash2(ix, iy, seed);
  b := Hash2(ix + 1, iy, seed);
  c := Hash2(ix, iy + 1, seed);
  d := Hash2(ix + 1, iy + 1, seed);
  Result := a + (b - a) * fx + (c - a) * fy + (a - b - c + d) * fx * fy;
end;

function Clamp255(v: Single): Integer;
begin
  if v < 0 then
    v := 0
  else if v > 255 then
    v := 255;
  Result := Round(v);
end;

function FBM(x, y: Single; seed, oct: Integer): Single;
var
  i: Integer;
  amp, sum, norm: Single;
begin
  amp := 0.5;
  sum := 0;
  norm := 0;
  for i := 0 to oct - 1 do
  begin
    sum := sum + amp * VNoise(x, y, seed + i * 17);
    norm := norm + amp;
    x := x * 2.03 + 11.7;
    y := y * 2.03 + 5.3;
    amp := amp * 0.5;
  end;
  Result := sum / norm;
end;

function SStep(e0, e1, x: Single): Single;
var
  t: Single;
begin
  t := (x - e0) / (e1 - e0);
  if t < 0 then
    t := 0
  else if t > 1 then
    t := 1;
  Result := t * t * (3 - 2 * t);
end;

function CrackDist(x, y: Single; seed: Integer): Single;
var
  ix, iy, i, j: Integer;
  px, py, d, d1, d2: Single;
begin
  ix := Floor(x);
  iy := Floor(y);
  d1 := 9;
  d2 := 9;
  for j := -1 to 1 do
    for i := -1 to 1 do
    begin
      px := ix + i + Hash2(ix + i, iy + j, seed);
      py := iy + j + Hash2(ix + i, iy + j, seed + 101);
      d := Sqrt(Sqr(px - x) + Sqr(py - y));
      if d < d1 then
      begin
        d2 := d1;
        d1 := d;
      end
      else if d < d2 then
        d2 := d;
    end;
  Result := d2 - d1;
end;

function ClampI(v, hi: Integer): Integer;
begin
  if v < 0 then
    Result := 0
  else if v > hi then
    Result := hi
  else
    Result := v;
end;

procedure BlurRowJob(y: Integer);
var
  x, i, o: Integer;
  sum: Double;
begin
  o := y * JbW;
  sum := 0;
  for i := -JbRad to JbRad do
    sum := sum + JbA^[o + ClampI(i, JbW - 1)];
  for x := 0 to JbW - 1 do
  begin
    JbT^[o + x] := sum * JbInv;
    sum := sum - JbA^[o + ClampI(x - JbRad, JbW - 1)] + JbA^[o + ClampI(x + JbRad + 1, JbW - 1)];
  end;
end;

procedure BlurColJob(s: Integer);
var
  x, x0, x1, y, i: Integer;
  sum: Double;
begin
  x0 := s * 16;
  x1 := x0 + 15;
  if x1 > JbW - 1 then
    x1 := JbW - 1;
  for x := x0 to x1 do
  begin
    sum := 0;
    for i := -JbRad to JbRad do
      sum := sum + JbT^[ClampI(i, JbH - 1) * JbW + x];
    for y := 0 to JbH - 1 do
    begin
      JbA^[y * JbW + x] := sum * JbInv;
      sum := sum - JbT^[ClampI(y - JbRad, JbH - 1) * JbW + x] + JbT^[ClampI(y + JbRad + 1, JbH - 1) * JbW + x];
    end;
  end;
end;

procedure BlurBuf(var a: TFloatBuf; W, H, rad: Integer);
var
  tmp: TFloatBuf;
  pass: Integer;
begin
  if rad < 1 then
    Exit;
  SetLength(tmp, W * H);
  JbA := @a[0];
  JbT := @tmp[0];
  JbW := W;
  JbH := H;
  JbRad := rad;
  JbInv := 1 / (2 * rad + 1);
  for pass := 1 to 3 do
  begin
    RunRowJob(BlurRowJob, H);
    RunRowJob(BlurColJob, (W + 15) div 16);
  end;
end;

procedure DirtNoiseRowJob(y: Integer);
var
  x, o: Integer;
  fx, fy, n1, n2: Single;
begin
  for x := 0 to JdW - 1 do
  begin
    o := y * JdW + x;
    fx := x;
    fy := y * 1.7;

    n1 := FBM(fx / 216.0, fy / 216.0, JdSeed + 1200, 3);
    n2 := FBM(fx / 5.5, fy / 5.5, JdSeed + 1300, 2);

    JdDirt^[o] := SStep(0.38, 0.68, n1 * 0.7 + n2 * 0.3);
  end;
end;

procedure DirtApplyRowJob(y: Integer);
var
  x, o: Integer;
  fx, fy, n2, clumpMask, alphaVal: Single;
  c: TColor32;
  r, g, b, dr, dg, db: Integer;
begin
  for x := 0 to JdW - 1 do
  begin
    o := y * JdW + x;
    clumpMask := JdDirt^[o];

    if clumpMask > 0.001 then
    begin
      fx := x;
      fy := y * 1.7;
      n2 := FBM(fx / 5.5, fy / 5.5, JdSeed + 1300, 2);

      dr := Clamp255(58 + n2 * 30);
      dg := Clamp255(42 + n2 * 22);
      db := Clamp255(24 + n2 * 14);

      alphaVal := clumpMask * 0.20;

      c := JdBits^[o];
      r := (c shr 16) and $FF;
      g := (c shr 8) and $FF;
      b := c and $FF;

      r := Clamp255(r + (dr - r) * alphaVal);
      g := Clamp255(g + (dg - g) * alphaVal);
      b := Clamp255(b + (db - b) * alphaVal);

      JdBits^[o] := Color32(r, g, b, 255);
    end;
  end;
end;

procedure DrawDirtOverlay(bmp: TBitmap32; Seed: Integer);
var
  W, H: Integer;
  mDirt: TFloatBuf;
  BlurDirt: Integer;
begin
  W := bmp.Width;
  H := bmp.Height;
  SetLength(mDirt, W * H);

  JdSeed := Seed;
  JdW := W;
  JdDirt := @mDirt[0];
  RunRowJob(DirtNoiseRowJob, H);

  BlurDirt := 3;
  BlurBuf(mDirt, W, H, BlurDirt);

  JdBits := PCardinalArray(@bmp.Bits[0]);
  RunRowJob(DirtApplyRowJob, H);
end;

procedure PutDot(bmp: TBitmap32; px, py, rx, ry, cr, cg, cb, al: Single);
var
  xi, yi, x0, x1, y0, y1, o: Integer;
  d, a: Single;
  c: Cardinal;
  qr, qg, qb: Integer;
begin
  x0 := Max(0, Floor(px - rx - 1));
  x1 := Min(bmp.Width - 1, Ceil(px + rx + 1));
  y0 := Max(0, Floor(py - ry - 1));
  y1 := Min(bmp.Height - 1, Ceil(py + ry + 1));
  for yi := y0 to y1 do
    for xi := x0 to x1 do
    begin
      d := Sqrt(Sqr((xi + 0.5 - px) / rx) + Sqr((yi + 0.5 - py) / ry));
      a := ((1 - d) * Min(rx, ry) + 0.5) * al;
      if a <= 0 then
        Continue;
      if a > 1 then
        a := 1;
      o := yi * bmp.Width + xi;
      c := bmp.Bits[o];
      qr := (c shr 16) and $FF;
      qg := (c shr 8) and $FF;
      qb := c and $FF;
      bmp.Bits[o] := Color32(Clamp255(qr + (cr - qr) * a),
                             Clamp255(qg + (cg - qg) * a),
                             Clamp255(qb + (cb - qb) * a), 255);
    end;
end;

procedure DrawStone(bmp: TBitmap32; cx, cy, rx, ry: Single);
const
  NC = 3;
var
  x, y, i, j, id, best, nt, hs, cnt, ns, yr, key, x0, x1, y0, y1, o: Integer;
  sx, sy, sl: array[0..39] of Single;
  cc: array[0..15] of Single;
  tl: array of Single;
  tk: array of Integer;
  cy1, cy2, ry1, ry2, u, dy, ang, rr, rr0, dist, lit, al, f, nn, r, g, b, d, d1, d2, td, yy, lb, tt, jt: Single;
  p1, p2, p3, p4, q1, q2, t1, t2, gl: Single;
  c: Cardinal;
  qr, qg, qb: Integer;

  procedure EvalTop(px, py: Integer; var lt, ds: Single; var kk: Integer);
  var
    uu, ddy, an, r0, e, ee, a, k, fa, secA, phic, ec, nx, ny, nzv: Single;
    band, sec, n: Integer;
  begin
    uu := (px - cx) / rx;
    ddy := (py - cy1) / ry1;
    an := ArcTan2(ddy, uu);
    r0 := Sqrt(uu * uu + ddy * ddy);
    if r0 > 1 then
      r0 := 1;
    e := ArcSin(r0);
    ee := e + 0.1 * Sin(3 * an + p3) + 0.06 * Sin(5 * an + p4);
    if ee < t1 then
    begin
      band := 0;
      ec := 0.25;
      n := NC;
    end;
    if (ee >= t1) and (ee < t2) then
    begin
      band := 1;
      ec := 0.78;
      n := nt;
    end;
    if ee >= t2 then
    begin
      band := 2;
      ec := 1.3;
      n := nt;
    end;
    secA := 2 * Pi / n;
    a := an + Pi + q2 + 0.3 * Sin(4 * e + q1);
    k := a / secA;
    sec := Floor(k);
    fa := k - sec;
    ds := Min(fa, 1 - fa) * secA * r0 * rx;
    ds := Min(ds, Min(Abs(ee - t1), Abs(ee - t2)) * rx * 0.8);
    phic := (sec + 0.5) * secA - Pi;
    nx := Sin(ec) * Cos(phic);
    ny := Sin(ec) * Sin(phic);
    nzv := Cos(ec);
    lt := -0.45 * nx - 0.6 * ny + 0.66 * nzv;
    kk := band * 16 + sec + 8;
  end;

begin
  PutDot(bmp, cx + rx * 0.25, cy + 0.5, rx * 1.35, ry * 0.5, 45, 50, 25, 0.4);

  nt := 6 + Random(3);
  hs := Random(1000);
  p1 := Random * 2 * Pi;
  p2 := Random * 2 * Pi;
  p3 := Random * 2 * Pi;
  p4 := Random * 2 * Pi;
  q1 := Random * 2 * Pi;
  q2 := Random * 0.6;
  t1 := 0.5 + (Random - 0.5) * 0.14;
  t2 := 1.05 + (Random - 0.5) * 0.14;
  gl := 1 + (Random - 0.5) * 0.16;

  cy1 := cy - ry * 0.2;
  cy2 := cy1 + ry * 0.8;
  ry1 := ry * 0.9;
  ry2 := ry * 0.5;

  x0 := Max(0, Floor(cx - rx * 1.3));
  x1 := Min(bmp.Width - 1, Ceil(cx + rx * 1.3));
  y0 := Max(0, Floor(cy1 - ry1 * 1.3));
  y1 := Min(bmp.Height - 1, Ceil(cy2 + ry2 * 1.3));

  SetLength(tl, bmp.Width);
  SetLength(tk, bmp.Width);

  yr := Trunc(cy1) - 1;
  for x := x0 to x1 do
    EvalTop(x, yr, tl[x], td, tk[x]);

  cnt := 0;
  for x := x0 + 1 to x1 do
    if (tk[x] <> tk[x - 1]) and (Abs(x - cx) < rx * 0.95) and (cnt < 12) then
      if (cnt = 0) or (x - 0.5 - cc[cnt - 1] > 14 * (rx / 80)) then
      begin
        cc[cnt] := x - 0.5;
        Inc(cnt);
      end;

  ns := 0;
  for i := 0 to cnt - 1 do
  begin
    yy := cy1 + ry1 * 0.12 + (Random - 0.5) * 6;
    sx[ns] := cc[i] - 7 * (rx / 80);
    sy[ns] := yy;
    sl[ns] := Random * 2 - 1;
    Inc(ns);
    sx[ns] := cc[i] + 7 * (rx / 80);
    sy[ns] := yy;
    sl[ns] := Random * 2 - 1;
    Inc(ns);
  end;
  for j := 0 to 1 do
    for i := 0 to 3 do
    begin
      sx[ns] := cx + (i - 1.5) * rx * 0.55 + (j mod 2) * rx * 0.2 + (Random - 0.5) * rx * 0.25;
      sy[ns] := cy1 + ry1 * 0.6 + j * (cy2 - cy1) * 0.8 + (Random - 0.5) * 14;
      sl[ns] := Random * 2 - 1;
      Inc(ns);
    end;

  for y := y0 to y1 do
    for x := x0 to x1 do
    begin
      u := (x - cx) / rx;
      if y < cy1 then
        dy := (y - cy1) / ry1;
      if (y >= cy1) and (y <= cy2) then
        dy := 0;
      if y > cy2 then
        dy := (y - cy2) / ry2;

      ang := ArcTan2(dy, u);
      rr0 := Sqrt(u * u + dy * dy);
      rr := rr0 / (1 + 0.04 * Sin(3 * ang + p1) + 0.03 * Sin(7 * ang + p2));
      al := Min(1, Max(0, (1 - rr) * rx / 1.5));
      if al <= 0 then
        Continue;

      if y < cy1 then
      begin
        EvalTop(x, y, lit, dist, key);
        id := key;
        tt := 1;
      end
      else
      begin
        d1 := 1E9;
        d2 := 1E9;
        best := 0;
        for i := 0 to ns - 1 do
        begin
          d := Sqrt(Sqr(x - sx[i]) + Sqr((y - sy[i]) * 0.85));
          if d < d1 then
          begin
            d2 := d1;
            d1 := d;
            best := i;
          end;
          if d < d2 then
            d2 := d;
        end;
        dist := (d2 - d1) * 0.5;
        lb := 0.4 - 0.18 * (sx[best] - cx) / rx + sl[best] * 0.1;
        lb := lb * (1 - 0.35 * (y - cy1) / Max(1.0, y1 - cy1));
        if y > cy2 then
          lb := lb * 0.85;
        tt := Min(1, (y - cy1) / Max(1.0, cy2 - cy1 + ry2));
        lit := tl[x] * (1 - tt) + lb * tt;
        id := 64 + best;
      end;

      jt := (((id * 73 + 19 + hs) mod 17) / 17 - 0.5) * 0.14 * tt;
      lit := lit * gl + jt;
      lit := Min(1, Max(0, lit));

      r := 28 + lit * 100;
      g := 34 + lit * 106;
      b := 48 + lit * 114;

      if dist < 1.4 then
      begin
        f := (1 - dist / 1.4) * 0.45;
        r := r + (165 - r) * f;
        g := g + (175 - g) * f;
        b := b + (188 - b) * f;
      end;

      if rr > 0.93 then
      begin
        f := Min(1, (rr - 0.93) / 0.07) * 0.5;
        r := r * (1 - f);
        g := g * (1 - f);
        b := b * (1 - f);
      end;

      nn := (Random - 0.5) * 6;
      o := y * bmp.Width + x;
      if al >= 1 then
        bmp.Bits[o] := Color32(Clamp255(r + nn), Clamp255(g + nn), Clamp255(b + nn), 255)
      else
      begin
        c := bmp.Bits[o];
        qr := (c shr 16) and $FF;
        qg := (c shr 8) and $FF;
        qb := c and $FF;
        bmp.Bits[o] := Color32(
          Clamp255(qr + (r + nn - qr) * al),
          Clamp255(qg + (g + nn - qg) * al),
          Clamp255(qb + (b - qb) * al), 255);
      end;
    end;
end;

procedure DrawPebble(bmp: TBitmap32; px, py, rs, cr, cg, cb: Single);
begin
  DrawStone(bmp, px, py, Max(2.5, rs), Max(1.8, rs * 0.75));
end;

procedure DrawBlade(bmp: TBitmap32; bx, by, ang, len, w0, tone: Single);
var
  i, steps: Integer;
  t, px, py, bend, w: Single;
begin
  steps := Round(len * 1.5);
  bend := (Random - 0.5) * len * 0.6;
  for i := 0 to steps do
  begin
    t := i / steps;
    px := bx + Sin(ang) * len * t + bend * t * t;
    py := by - Cos(ang) * len * t;
    w := w0 * (1 - t) + 0.4;
    PutDot(bmp, px, py, w, w, 55 + 95 * t + tone, 100 + 100 * t + tone, 34 + 36 * t, 1);
  end;
end;

procedure DrawTuft(bmp: TBitmap32; tx, ty: Single);
var
  k, nb: Integer;
begin
  nb := 5 + Random(5);
  for k := 1 to nb do
    DrawBlade(
      bmp, 
      tx + (Random - 0.5) * 2,
      ty + (Random - 0.5) * 0.75,
      (Random - 0.5) * 2.2,
      3 + Random * 2,
      1.7 + Random * 0.5,
      (Random - 0.5) * 30
    );
end;

procedure DrawLeaf(bmp: TBitmap32; bx, by, ang, len, w0, droop, tone: Single);
var
  i, steps: Integer;
  t, px, py, w: Single;
begin
  steps := Round(len * 1.6);
  for i := 0 to steps do
  begin
    t := i / steps;
    px := bx + Sin(ang) * len * t;
    py := by - Cos(ang) * len * t + droop * len * t * t;
    w := 0.45 + w0 * Sin(Pi * t) * (1 - 0.35 * t);
    PutDot(bmp, px, py, w, w, 34 + 40 * t + tone, 84 + 70 * t + tone, 30 + 18 * t, 1);
  end;
  for i := 0 to steps do
  begin
    t := i / steps;
    px := bx + Sin(ang) * len * t;
    py := by - Cos(ang) * len * t + droop * len * t * t;
    w := 0.45 + w0 * Sin(Pi * t) * (1 - 0.35 * t);
    PutDot(bmp, px - w * 0.3, py - w * 0.35, w * 0.5, w * 0.45,
      96 + 60 * t + tone, 170 + 40 * t + tone, 70 + 20 * t, 0.55);
  end;
end;

procedure DrawClump(bmp: TBitmap32; cx, cy: Single);
var
  i, n: Integer;
  rx, ry, ang, dxp: Single;
begin
  rx := 2.5 + Random * 2.5;
  ry := rx * (0.75 + Random * 0.2);

  n := 8 + Random(5);
  for i := 1 to n do
  begin
    ang := (Random - 0.5) * 2.6;
    DrawLeaf(bmp, cx + (Random - 0.5) * rx, cy - ry * 0.6, ang, 
      6 + Random * 20,
      0.7 + Random * 2.45,
      0.2 + 0.4 * Abs(ang) + Random * 0.3, (Random - 0.5) * 24);
  end;

  DrawStone(bmp, cx, cy, rx, ry);

  n := 6 + Random(4);
  for i := 1 to n do
  begin
    ang := (Random - 0.5) * 3;
    DrawLeaf(bmp, cx + (Random - 0.5) * rx * 1.6, cy - ry * 0.1, ang, 
      4.5 + Random * 20,
      0.65 + Random * 2.45,
      0.2 + 0.4 * Abs(ang) + Random * 0.3, (Random - 0.5) * 24);
  end;

  n := 3 + Random(3);
  for i := 1 to n do
  begin
    dxp := (rx * 1.4 + Random * rx * 2.5) * (1 - 2 * Random(2));
    DrawPebble(bmp, cx + dxp, cy + Random * ry * 0.8 + 1, 1.25 + Random * 1.0, 150, 156, 168);
  end;
end;

constructor TMossPass1Thread.Create(YStart, YEnd, W, Seed: Integer; ISO, MossLevel: Single;
  ABufT, AMC, AMM, AMT, AMD: Pointer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FYStart := YStart;
  FYEnd := YEnd;
  FW := W;
  FSeed := Seed;
  FISO := ISO;
  FMossLevel := MossLevel;
  PbufT := ABufT;
  PmC := AMC;
  PmM := AMM;
  PmT := AMT;
  PmD := AMD;
  Resume;
end;

procedure TMossPass1Thread.Execute;
var
  x, y, o: Integer;
  fx, fy, wx, wy, mvv, nt, nd, core, cd: Single;
begin
  while True do
  begin
    y := InterlockedIncrement(MossRowNext) - 1;
    if y > FYEnd then
      Break;
    for x := 0 to FW - 1 do
    begin
      o := y * FW + x;
      fx := x;
      fy := y * FISO;

      wx := FBM(fx / 90 + 3.1, fy / 90 + 7.7, FSeed, 3) - 0.5;
      wy := FBM(fx / 90 + 9.4, fy / 90 + 1.2, FSeed + 50, 3) - 0.5;
      mvv := FBM(fx / 70 + wx * 1.8, fy / 70 + wy * 1.8, FSeed + 200, 4);

      PmM^[o] := SStep(FMossLevel, FMossLevel + 0.06, mvv);
      nt := FBM(fx / 34 + 70, fy / 34 + 70, FSeed + 900, 3);
      PmT^[o] := SStep(0.5, 0.58, nt) * SStep(FMossLevel - 0.05, FMossLevel + 0.03, mvv);
      nd := FBM(fx / 22 + 30, fy / 22 + 30, FSeed + 600, 3);
      core := SStep(FMossLevel + 0.05, FMossLevel + 0.06, mvv);
      PmD^[o] := core * SStep(0.585, 0.6, nd);

      PbufT^[o] := SStep(0.3, 0.7, FBM(fx / 45 + 20, fy / 45 + 20, FSeed + 400, 3));
      cd := CrackDist(fx / 26, fy / 26, FSeed + 800);
      PmC^[o] := 1 - SStep(0, 0.12, cd);
    end;
  end;
end;

constructor TMossPass3Thread.Create(YStart, YEnd, W, Seed: Integer; ISO, MossLevel,
  OpRetak, OpMuda, OpTerang, OpTua: Single;
  ABufT, AMC, AMM, AMT, AMD, ABits: Pointer);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FYStart := YStart;
  FYEnd := YEnd;
  FW := W;
  FSeed := Seed;
  FISO := ISO;
  FMossLevel := MossLevel;
  FOpRetak := OpRetak;
  FOpMuda := OpMuda;
  FOpTerang := OpTerang;
  FOpTua := OpTua;
  PbufT := ABufT;
  PmC := AMC;
  PmM := AMM;
  PmT := AMT;
  PmD := AMD;
  PBits := ABits;
  Resume;
end;

procedure TMossPass3Thread.Execute;
var
  x, y, o: Integer;
  fx, fy, t, grain, a, r, g, b: Single;
begin
  while True do
  begin
    y := InterlockedIncrement(MossRowNext) - 1;
    if y > FYEnd then
      Break;
    for x := 0 to FW - 1 do
    begin
      o := y * FW + x;
      fx := x;
      fy := y * FISO;

      t := PbufT^[o];
      grain := (VNoise(fx / 1.8, fy / 1.8, FSeed + 500) - 0.5) * 26;
      r := 194 + 30 * t + grain;
      g := 142 + 36 * t + grain * 0.9;
      b := 54 + 34 * t + grain * 0.6;

      a := PmC^[o] * FOpRetak;
      r := r + (112 - r) * a;
      g := g + (74 - g) * a;
      b := b + (32 - b) * a;

      if (PmM^[o] > 0.003) or (PmT^[o] > 0.003) or (PmD^[o] > 0.003) then
      begin
        t := SStep(0.3, 0.7, FBM(fx / 16 + 50, fy / 16 + 50, FSeed + 650, 3));
        grain := (VNoise(fx / 1.5, fy / 1.5, FSeed + 700) - 0.5) * 30;

        a := PmM^[o] * FOpMuda;
        r := r + (90 + 40 * t + grain * 0.8 - r) * a;
        g := g + (138 + 24 * t + grain - g) * a;
        b := b + (34 + 12 * t + grain * 0.4 - b) * a;

        a := PmT^[o] * FOpTerang;
        r := r + (200 + grain * 0.6 - r) * a;
        g := g + (200 + grain * 0.6 - g) * a;
        b := b + (66 + grain * 0.3 - b) * a;

        a := PmD^[o] * FOpTua;
        r := r + (52 + 30 * t + grain * 0.8 - r) * a;
        g := g + (96 + 30 * t + grain - g) * a;
        b := b + (30 + 10 * t + grain * 0.4 - b) * a;
      end;

      PBits^[o] := Color32(Clamp255(r), Clamp255(g), Clamp255(b), 255);
    end;
  end;
end;

procedure SaveToJPEG(bmp: TBitmap32; const FileName: String; Quality: Integer);
var
  Jpg: TJPEGImage;
  TmpBmp: Graphics.TBitmap;
begin
  TmpBmp := Graphics.TBitmap.Create;
  Jpg := TJPEGImage.Create;
  try
    TmpBmp.Assign(bmp);
    Jpg.CompressionQuality := Quality;
    Jpg.Assign(TmpBmp);
    Jpg.SaveToFile(FileName);
  finally
    Jpg.Free;
    TmpBmp.Free;
  end;
end;

const
  TR_MAXOBJ = 12000;
  TR_LX = 0.66;
  TR_LY = -0.75;
  TR_SX = -0.72;
  TR_SY = 0.69;
  TR_NPAL = 8;
  TR_CELL_TREE = 92;
  TR_CELL_UNDER = 46;
  TR_CELL_OPEN = 150;
  TR_CELL_TUFT = 56;
  TR_CELL_ROCKF = 210;
  TR_CELL_COB = 40;
  TR_BAND = 24;
  TR_OLD_MOSS_CHANCE = 0.0;

type
  TTrObj = record
    Kind, Sub, Seed: Integer;
    X, Y, Sz, Z: Single;
  end;

var
  TrObj: array[0..TR_MAXOBJ - 1] of TTrObj;
  TrOrd: array[0..TR_MAXOBJ - 1] of Integer;
  TrN: Integer;
  TrPal: array[0..TR_NPAL * 7 * 3 - 1] of Single;
  TrProf: array[0..256] of Single;
  TrPR, TrPG, TrPB, TrSH, TrFF, TrPF, TrGN: TFloatBuf;
  TrW, TrH, TrGW, TrGH, TrGX0, TrGY0: Integer;
  TrWX, TrWY: Single;
  TrSeed: Integer;

threadvar
  TrMode, TrY0, TrY1: Integer;
  TrShA, TrCR, TrCG, TrCB, TrPM, TrPE, TrSoft: Single;

function TrMin(a, b: Single): Single;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function TrMax(a, b: Single): Single;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function TrClamp(v, lo, hi: Single): Single;
begin
  if v < lo then
    Result := lo
  else if v > hi then
    Result := hi
  else
    Result := v;
end;

function TrHsh(i, j, salt: Integer): Single;
begin
  Result := Hash2(i, j, TrSeed + salt);
end;

function TrCY(v: Integer): Integer;
begin
  if v < TrY0 then
    Result := TrY0
  else if v > TrY1 then
    Result := TrY1
  else
    Result := v;
end;

procedure TrSetPal(v, k, r, g, b: Integer);
var
  o: Integer;
begin
  o := (v * 7 + k) * 3;
  TrPal[o] := r;
  TrPal[o + 1] := g;
  TrPal[o + 2] := b;
end;

procedure TrInitPal;
var
  i: Integer;
begin
  for i := 0 to 256 do
    TrProf[i] := Power(TrMax(0, Sin(Pi * Power(i / 256, 0.8))), 0.75);
  TrSetPal(0, 0, 11, 65, 28);
  TrSetPal(0, 1, 24, 98, 53);
  TrSetPal(0, 2, 40, 116, 46);
  TrSetPal(0, 3, 64, 140, 50);
  TrSetPal(0, 4, 92, 164, 54);
  TrSetPal(0, 5, 128, 192, 60);
  TrSetPal(0, 6, 165, 214, 78);

  TrSetPal(1, 0, 22, 76, 15);
  TrSetPal(1, 1, 46, 113, 23);
  TrSetPal(1, 2, 77, 141, 17);
  TrSetPal(1, 3, 108, 167, 18);
  TrSetPal(1, 4, 139, 189, 20);
  TrSetPal(1, 5, 170, 208, 29);
  TrSetPal(1, 6, 196, 224, 60);

  TrSetPal(2, 0, 30, 73, 26);
  TrSetPal(2, 1, 46, 110, 42);
  TrSetPal(2, 2, 79, 102, 18);
  TrSetPal(2, 3, 91, 137, 30);
  TrSetPal(2, 4, 131, 152, 25);
  TrSetPal(2, 5, 170, 183, 33);
  TrSetPal(2, 6, 205, 200, 62);

  TrSetPal(3, 0, 21, 65, 25);
  TrSetPal(3, 1, 38, 95, 33);
  TrSetPal(3, 2, 64, 125, 33);
  TrSetPal(3, 3, 89, 149, 30);
  TrSetPal(3, 4, 115, 169, 29);
  TrSetPal(3, 5, 142, 186, 32);
  TrSetPal(3, 6, 176, 208, 52);

  TrSetPal(4, 0, 25, 76, 23);
  TrSetPal(4, 1, 50, 116, 26);
  TrSetPal(4, 2, 70, 152, 35);
  TrSetPal(4, 3, 109, 174, 34);
  TrSetPal(4, 4, 142, 187, 50);
  TrSetPal(4, 5, 170, 205, 80);
  TrSetPal(4, 6, 196, 226, 104);

  TrSetPal(5, 0, 60, 74, 86);
  TrSetPal(5, 1, 73, 84, 94);
  TrSetPal(5, 2, 96, 106, 112);
  TrSetPal(5, 3, 122, 124, 121);
  TrSetPal(5, 4, 152, 147, 138);
  TrSetPal(5, 5, 182, 176, 162);
  TrSetPal(5, 6, 208, 200, 182);

  TrSetPal(6, 0, 120, 66, 24);
  TrSetPal(6, 1, 160, 82, 30);
  TrSetPal(6, 2, 199, 104, 38);
  TrSetPal(6, 3, 209, 118, 46);
  TrSetPal(6, 4, 219, 138, 62);
  TrSetPal(6, 5, 237, 156, 75);
  TrSetPal(6, 6, 246, 182, 102);

  TrSetPal(7, 0, 58, 44, 20);
  TrSetPal(7, 1, 82, 64, 26);
  TrSetPal(7, 2, 98, 76, 32);
  TrSetPal(7, 3, 114, 88, 37);
  TrSetPal(7, 4, 135, 105, 47);
  TrSetPal(7, 5, 155, 124, 60);
  TrSetPal(7, 6, 172, 141, 75);
end;

procedure TrRamp(v: Integer; t: Single);
var
  f: Single;
  k, o1, o2: Integer;
begin
  f := TrClamp(t, 0, 0.9999) * 6;
  k := Floor(f);
  f := f - k;
  o1 := (v * 7 + k) * 3;
  o2 := o1 + 3;
  TrCR := TrPal[o1] + (TrPal[o2] - TrPal[o1]) * f;
  TrCG := TrPal[o1 + 1] + (TrPal[o2 + 1] - TrPal[o1 + 1]) * f;
  TrCB := TrPal[o1 + 2] + (TrPal[o2 + 2] - TrPal[o1 + 2]) * f;
end;

procedure TrPlot(o: Integer; cov, r, g, b: Single);
begin
  if TrMode = 1 then
  begin
    cov := cov * TrShA;
    if cov > TrSH[o] then
      TrSH[o] := cov;
  end
  else
  begin
    TrPR[o] := TrPR[o] + (r - TrPR[o]) * cov;
    TrPG[o] := TrPG[o] + (g - TrPG[o]) * cov;
    TrPB[o] := TrPB[o] + (b - TrPB[o]) * cov;
  end;
end;

procedure TrDisk(cx, cy, rad, r, g, b, a: Single);
var
  xi, yi, x0, x1, y0, y1: Integer;
  d, cov: Single;
begin
  if (cx + rad < 0) or (cx - rad > TrW) then
    Exit;
  y0 := Floor(cy - rad - 1);
  y1 := Ceil(cy + rad + 1);
  if (y1 < TrY0) or (y0 > TrY1) then
    Exit;
  y0 := TrCY(y0);
  y1 := TrCY(y1);
  x0 := ClampI(Floor(cx - rad - 1), TrW - 1);
  x1 := ClampI(Ceil(cx + rad + 1), TrW - 1);
  for yi := y0 to y1 do
    for xi := x0 to x1 do
    begin
      d := Sqrt(Sqr(xi + 0.5 - cx) + Sqr(yi + 0.5 - cy));
      cov := TrClamp(rad - d + 0.5, 0, 1) * a;
      if cov > 0.003 then
        TrPlot(yi * TrW + xi, cov, r, g, b);
    end;
end;

procedure TrEll(cx, cy, rx, ry, r, g, b, a: Single);
var
  xi, yi, x0, x1, y0, y1: Integer;
  d, cov, mr: Single;
begin
  if (cx + rx < 0) or (cx - rx > TrW) then
    Exit;
  y0 := Floor(cy - ry - 1);
  y1 := Ceil(cy + ry + 1);
  if (y1 < TrY0) or (y0 > TrY1) then
    Exit;
  y0 := TrCY(y0);
  y1 := TrCY(y1);
  x0 := ClampI(Floor(cx - rx - 1), TrW - 1);
  x1 := ClampI(Ceil(cx + rx + 1), TrW - 1);
  mr := TrMin(rx, ry);
  for yi := y0 to y1 do
    for xi := x0 to x1 do
    begin
      d := (Sqrt(Sqr((xi + 0.5 - cx) / rx) + Sqr((yi + 0.5 - cy) / ry)) - 1) * mr;
      cov := TrClamp(0.5 - d, 0, 1) * a;
      if cov > 0.003 then
        TrPlot(yi * TrW + xi, cov, r, g, b);
    end;
end;

procedure TrCap(x0, y0, x1, y1, w0, w1, r, g, b, a: Single);
var
  xi, yi, xa, xb, ya, yb: Integer;
  vx, vy, l2, t, qx, qy, d, hw, cov, wm: Single;
begin
  wm := TrMax(w0, w1) * 0.5 + 1;
  xa := ClampI(Floor(TrMin(x0, x1) - wm), TrW - 1);
  xb := ClampI(Ceil(TrMax(x0, x1) + wm), TrW - 1);
  ya := Floor(TrMin(y0, y1) - wm);
  yb := Ceil(TrMax(y0, y1) + wm);
  if (yb < TrY0) or (ya > TrY1) then
    Exit;
  ya := TrCY(ya);
  yb := TrCY(yb);
  vx := x1 - x0;
  vy := y1 - y0;
  l2 := vx * vx + vy * vy;
  for yi := ya to yb do
    for xi := xa to xb do
    begin
      qx := xi + 0.5 - x0;
      qy := yi + 0.5 - y0;
      if l2 < 0.000001 then
        t := 0
      else
        t := TrClamp((qx * vx + qy * vy) / l2, 0, 1);
      d := Sqrt(Sqr(qx - t * vx) + Sqr(qy - t * vy));
      hw := (w0 + (w1 - w0) * t) * 0.5;
      cov := TrClamp(hw - d + 0.5, 0, 1) * a;
      if cov > 0.003 then
        TrPlot(yi * TrW + xi, cov, r, g, b);
    end;
end;

procedure TrLeaf(bx, by, ang, len, wid, bend, lr, lg, lb, sr, sg, sb, rib, serr, a: Single);
var
  ca, sa, tx, ty, mx, my, lsg, dx, dy, t, tc, s, wp, e, cov, u, m, tt, cr, cg, cb, ed, rm, ax, pf: Single;
  xa, xb, ya, yb, xi, yi, pi0: Integer;
begin
  ca := Cos(ang);
  sa := Sin(ang);
  tx := bx + ca * len - sa * bend * len;
  ty := by + sa * len + ca * bend * len;
  mx := bx + ca * len * 0.5 - sa * bend * len * 0.25;
  my := by + sa * len * 0.5 + ca * bend * len * 0.25;
  xa := ClampI(Floor(TrMin(bx, TrMin(tx, mx)) - wid - 2), TrW - 1);
  xb := ClampI(Ceil(TrMax(bx, TrMax(tx, mx)) + wid + 2), TrW - 1);
  ya := Floor(TrMin(by, TrMin(ty, my)) - wid - 2);
  yb := Ceil(TrMax(by, TrMax(ty, my)) + wid + 2);
  if (TrMax(bx, TrMax(tx, mx)) + wid < 0) or (TrMin(bx, TrMin(tx, mx)) - wid > TrW) then
    Exit;
  if (yb < TrY0) or (ya > TrY1) then
    Exit;
  ya := TrCY(ya);
  yb := TrCY(yb);
  lsg := (-sa) * TR_LX + ca * TR_LY;
  for yi := ya to yb do
    for xi := xa to xb do
    begin
      dx := xi + 0.5 - bx;
      dy := yi + 0.5 - by;
      t := (dx * ca + dy * sa) / len;
      if (t < -0.03) or (t > 1.03) then
        Continue;
      s := (-dx * sa + dy * ca) - bend * len * t * t;
      tc := TrClamp(t, 0, 1);
      pf := tc * 256;
      pi0 := Floor(pf);
      if pi0 > 255 then
        pi0 := 255;
      pf := pf - pi0;
      wp := wid * (TrProf[pi0] + (TrProf[pi0 + 1] - TrProf[pi0]) * pf) * (1 - 0.14 * tc);
      if serr > 0 then
        wp := wp * (1 - serr * 0.5 * (0.5 + 0.5 * Sin(tc * len * 1.7)));
      ax := Abs(s);
      e := wp - ax;
      cov := TrClamp(e + 0.5, 0, 1);
      tt := TrMin(t * len, (1 - t) * len);
      cov := cov * TrClamp(tt + 0.5, 0, 1) * a;
      if cov <= 0.003 then
        Continue;
      if TrMode = 1 then
      begin
        TrPlot(yi * TrW + xi, cov, 0, 0, 0);
        Continue;
      end;
      if lsg >= 0 then
        u := s / (wp + 0.001)
      else
        u := -s / (wp + 0.001);
      m := SStep(-TrSoft, TrSoft, u);
      cr := sr + (lr - sr) * m;
      cg := sg + (lg - sg) * m;
      cb := sb + (lb - sb) * m;
      ed := 1 - 0.16 * SStep(0.65, 1.0, ax / (wp + 0.01));
      cr := cr * ed;
      cg := cg * ed;
      cb := cb * ed;
      if rib > 0 then
      begin
        rm := rib * SStep(1.0, 0.15, ax) * SStep(0.04, 0.2, tc) * SStep(1.0, 0.65, tc);
        cr := cr + (lr + 38 - cr) * rm;
        cg := cg + (lg + 30 - cg) * rm;
        cb := cb + (lb + 8 - cb) * rm;
      end;
      TrPlot(yi * TrW + xi, cov, cr, cg, cb);
    end;
end;

procedure TrLeafR(bx, by, ang, len, wid, bend, tone, rib, serr, a, dc: Single; pv: Integer);
var
  lr, lg, lb: Single;
begin
  TrRamp(pv, tone + dc);
  lr := TrCR;
  lg := TrCG;
  lb := TrCB;
  TrRamp(pv, tone - dc);
  TrLeaf(bx, by, ang, len, wid, bend, lr, lg, lb, TrCR, TrCG, TrCB, rib, serr, a);
end;

threadvar
  TrOS: Integer;

function TrOH(a, b, c: Integer): Single;
begin
  Result := Hash2(a * 977 + b, c, TrOS);
end;

function TrOut(ang: Single): Single;
begin
  Result := 0.93 + 0.06 * Sin(3 * ang + TrOH(9, 1, 1) * 6.28) + 0.045 * Sin(5 * ang + TrOH(9, 2, 1) * 6.28) + 0.025 * Sin(9 * ang + TrOH(9, 3, 1) * 6.28);
end;

procedure TrDrawRock(bx, by, r: Single; nf, sd: Integer);
var
  fx, fy, fnx, fny, fnz, ft: array[0..15] of Single;
  k, xi, yi, xa, xb, ya, yb, k1: Integer;
  cx, cy, ry, d, dx, dy, rho, ang, rr, cov, d1, d2, t, lam, nl, px, py, dist, ph1, ph2, edg, rim, best: Single;
begin
  TrOS := sd;
  if nf > 16 then
    nf := 16;
  cx := bx;
  cy := by - r * 0.85;
  ry := 0.9;
  if TrMode = 1 then
  begin
    TrShA := 0.9;
    TrEll(cx + TR_SX * 0.55 * r, cy + TR_SY * 0.5 * r, r * 1.0, r * 0.9, 0, 0, 0, 1);
    Exit;
  end;
  ph1 := TrOH(1, 1, 1) * 6.28;
  ph2 := TrOH(1, 2, 1) * 6.28;
  best := -9;
  for k := 0 to nf - 1 do
  begin
    if k = 0 then
    begin
      fx[k] := 0.12;
      fy[k] := -0.14;
    end
    else
    begin
      ang := TrOH(2, k, 1) * 6.28;
      d := 0.28 + 0.66 * Sqrt(TrOH(2, k, 2));
      fx[k] := Cos(ang) * d;
      fy[k] := Sin(ang) * d;
    end;
    d := Sqrt(fx[k] * fx[k] + fy[k] * fy[k]);
    fnx[k] := fx[k] * 0.95 + (TrOH(3, k, 1) - 0.5) * 0.55;
    fny[k] := fy[k] * 0.95 + (TrOH(3, k, 2) - 0.5) * 0.55;
    fnz[k] := Sqrt(TrMax(0.15, 1 - d * d * 0.8)) + (TrOH(3, k, 3) - 0.5) * 0.3;
    nl := Sqrt(fnx[k] * fnx[k] + fny[k] * fny[k] + fnz[k] * fnz[k]);
    fnx[k] := fnx[k] / nl;
    fny[k] := fny[k] / nl;
    fnz[k] := fnz[k] / nl;
    lam := fnx[k] * 0.55 - fny[k] * 0.62 + fnz[k] * 0.56;
    ft[k] := TrClamp(0.42 + 0.66 * (lam - 0.45) + (TrOH(3, k, 4) - 0.5) * 0.10, 0, 1);
    if ft[k] > best then
    begin
      best := ft[k];
      k1 := k;
    end;
  end;
  ft[k1] := TrMin(1, ft[k1] + 0.14);
  xa := ClampI(Floor(cx - r * 1.05 - 1), TrW - 1);
  xb := ClampI(Ceil(cx + r * 1.05 + 1), TrW - 1);
  ya := Floor(cy - r * ry * 1.05 - 1);
  yb := Ceil(cy + r * ry * 1.05 + 1);
  if (yb < TrY0) or (ya > TrY1) then
    Exit;
  ya := TrCY(ya);
  yb := TrCY(yb);
  for yi := ya to yb do
    for xi := xa to xb do
    begin
      dx := (xi + 0.5 - cx) / r;
      dy := (yi + 0.5 - cy) / (r * ry);
      rho := Sqrt(dx * dx + dy * dy);
      if rho > 1.15 then
        Continue;
      ang := ArcTan2(dy, dx);
      rr := 0.93 + 0.06 * Sin(3 * ang + ph1) + 0.04 * Sin(5 * ang + ph2);
      cov := TrClamp((rr - rho) * r + 0.5, 0, 1);
      if cov <= 0.003 then
        Continue;
      d1 := 9;
      d2 := 9;
      k1 := 0;
      for k := 0 to nf - 1 do
      begin
        d := Sqrt(Sqr(dx - fx[k]) + Sqr(dy - fy[k]));
        if d < d1 then
        begin
          d2 := d1;
          d1 := d;
          k1 := k;
        end
        else if d < d2 then
          d2 := d;
      end;
      t := ft[k1];
      rim := (dx * TR_SX + dy * TR_SY) / (rho + 0.001);
      t := t - 0.20 * SStep(0.72, 1.0, rho) * (0.5 + 0.5 * rim);
      TrRamp(5, t);
      edg := 1;
      if d2 - d1 < 0.07 then
        edg := 0.88;
      TrPlot(yi * TrW + xi, cov, TrCR * edg, TrCG * edg, TrCB * edg);
    end;
end;

procedure TrClump(px, py, rc, tone: Single; pv: Integer; a0: Single; nl: Integer);
var
  k: Integer;
  a, ln, ld, tn, bnd: Single;
begin
  for k := 0 to nl - 1 do
  begin
    a := a0 + k * 6.2832 / nl + (TrOH(5, k, 11) - 0.5) * 0.5;
    ld := Cos(a) * TR_LX + Sin(a) * TR_LY;
    tn := tone + 0.13 * ld + 0.10 * (TrOH(5, k, 12) - 0.5);
    ln := rc * (0.85 + 0.3 * TrOH(5, k, 13));
    bnd := (TrOH(5, k, 14) - 0.5) * 0.5;
    TrLeafR(px, py, a, ln, ln * 0.45, bnd, tn, 0.28, 0, 1, 0.085, pv);
  end;
end;

procedure TrCanopy(cx, cy, R, rc: Single; pv, pv2: Integer; mixp, tbias, shd: Single);
var
  l, i, nl, pvv: Integer;
  ri, ro, ang, rad, px, py, ld, tone, ry, o: Single;
begin
  ry := 0.93;
  if TrMode = 0 then
  begin
    TrRamp(pv, 0.03);
    TrDisk(cx, cy, R * 0.78, TrCR, TrCG, TrCB, 1);
    for i := 0 to 13 do
    begin
      ang := i * 6.2832 / 14;
      o := TrOut(ang);
      TrRamp(pv, 0.04 + 0.05 * (Cos(ang) * TR_LX + Sin(ang) * TR_LY));
      TrDisk(cx + Cos(ang) * R * 0.62 * o, cy + Sin(ang) * R * 0.62 * o * ry, R * 0.4 * o, TrCR, TrCG, TrCB, 1);
    end;
  end;
  for l := 0 to 3 do
  begin
    if l = 0 then
    begin
      ri := 0.5;
      ro := 1.0;
    end
    else if l = 1 then
    begin
      ri := 0.32;
      ro := 0.88;
    end
    else if l = 2 then
    begin
      ri := 0.1;
      ro := 0.66;
    end
    else
    begin
      ri := 0;
      ro := 0.4;
    end;
    if (TrMode = 1) and (l > 1) then
      Break;
    nl := Round((ro * ro - ri * ri) * R * R / (rc * rc) * 1.7) + 2;
    for i := 0 to nl - 1 do
    begin
      ang := TrOH(l, i, 1) * 6.2832;
      rad := Sqrt(ri * ri + TrOH(l, i, 2) * (ro * ro - ri * ri)) * R * TrOut(ang);
      px := cx + Cos(ang) * rad;
      py := cy + Sin(ang) * rad * ry;
      if TrMode = 1 then
      begin
        TrDisk(px + TR_SX * shd, py + TR_SY * shd, rc * 1.05, 0, 0, 0, 1);
        Continue;
      end;
      ld := ((px - cx) * TR_LX + (py - cy) * TR_LY) / R;
      tone := 0.10 + 0.15 * l + 0.36 * ld + 0.18 * (TrOH(l, i, 3) - 0.5) + tbias;
      if TrOH(l, i, 4) < mixp then
        pvv := pv2
      else
        pvv := pv;
      TrClump(px, py, rc * (0.85 + 0.3 * TrOH(l, i, 5)), tone, pvv, TrOH(l, i, 6) * 6.2832, 5 + Floor(TrOH(l, i, 7) * 4));
    end;
  end;
end;

procedure TrDrawTree(bx, by, R: Single; pv, sd: Integer);
var
  cx, cy, rc, w0, w1, tx, ty: Single;
begin
  TrOS := sd;
  cy := by - 1.3 * R;
  cx := bx + (TrOH(0, 0, 9) - 0.5) * 0.18 * R;
  rc := 6.4 + R * 0.04;
  if TrMode = 1 then
  begin
    TrShA := 1.0;
    TrCanopy(cx, cy, R, rc, pv, pv, 0, 0, R * 0.40);
    TrCap(bx, by, bx + TR_SX * R * 0.5, by + TR_SY * R * 0.4, R * 0.16, R * 0.10, 0, 0, 0, 1);
    Exit;
  end;
  w0 := R * 0.21;
  w1 := R * 0.14;
  tx := bx + (cx - bx) * 0.55;
  ty := by - 1.05 * R;
  TrRamp(7, 0.18);
  TrCap(bx - w0 * 0.55, by + 2, bx - w0 * 1.15, by + 4, w0 * 0.5, w0 * 0.3, TrCR, TrCG, TrCB, 1);
  TrCap(bx + w0 * 0.55, by + 2, bx + w0 * 1.15, by + 4, w0 * 0.5, w0 * 0.3, TrCR, TrCG, TrCB, 1);
  TrCap(bx, by, tx, ty, w0, w1, TrCR, TrCG, TrCB, 1);
  TrRamp(7, 0.62);
  TrCap(bx + w0 * 0.2, by - 1, tx + w1 * 0.2, ty, w0 * 0.5, w1 * 0.5, TrCR, TrCG, TrCB, 1);
  TrRamp(7, 0.92);
  TrCap(bx + w0 * 0.33, by - 2, tx + w1 * 0.33, ty, w0 * 0.16, w1 * 0.16, TrCR, TrCG, TrCB, 0.85);
  TrRamp(7, 0.30);
  TrCap(tx, ty, cx - R * 0.35, cy + R * 0.25, R * 0.09, R * 0.05, TrCR, TrCG, TrCB, 1);
  TrRamp(7, 0.52);
  TrCap(tx, ty, cx + R * 0.36, cy + R * 0.22, R * 0.09, R * 0.05, TrCR, TrCG, TrCB, 1);
  if pv = 2 then
    TrCanopy(cx, cy, R, rc, 2, 6, 0.14, 0, 0)
  else
    TrCanopy(cx, cy, R, rc, pv, pv, 0, 0, 0);
end;

procedure TrDrawPalm(bx, by, R: Single; sd: Integer);
var
  nf, k, j, kk: Integer;
  a, ln, wd, bnd, ld, tone, cx, cy, s, sh, tw: Single;
  ang: array[0..11] of Single;
  key: array[0..11] of Single;
  ord: array[0..11] of Integer;
  tmp: Integer;
  lr, lg, lb: Single;
begin
  TrOS := sd;
  cy := by - 1.1 * R;
  cx := bx;
  nf := 9 + Floor(TrOH(0, 0, 1) * 3);
  a := TrOH(0, 0, 2) * 6.2832;
  for k := 0 to nf - 1 do
  begin
    ang[k] := a + k * 6.2832 / nf + (TrOH(1, k, 1) - 0.5) * 0.45;
    key[k] := Sin(ang[k]) + (TrOH(1, k, 2) - 0.5) * 0.5;
    ord[k] := k;
  end;
  for k := 1 to nf - 1 do
  begin
    tmp := ord[k];
    j := k - 1;
    while (j >= 0) and (key[ord[j]] > key[tmp]) do
    begin
      ord[j + 1] := ord[j];
      j := j - 1;
    end;
    ord[j + 1] := tmp;
  end;
  if TrMode = 1 then
  begin
    TrShA := 0.95;
    sh := R * 0.42;
    TrCap(bx, by, cx + TR_SX * sh * 0.6, cy + 0.15 * R + TR_SY * sh * 0.6, R * 0.08, R * 0.06, 0, 0, 0, 1);
    for kk := 0 to nf - 1 do
    begin
      k := ord[kk];
      ln := R * (0.86 + 0.2 * TrOH(2, k, 1));
      wd := ln * 0.31;
      bnd := 0.22 * (TrOH(2, k, 2) - 0.5) * 2 + 0.16;
      TrLeaf(cx + TR_SX * sh, cy + TR_SY * sh, ang[k], ln, wd, bnd, 0, 0, 0, 0, 0, 0, 0, 0.3, 1);
    end;
    Exit;
  end;
  tw := R * 0.085 + 2;
  TrRamp(7, 0.14);
  TrCap(bx, by, cx, cy + 0.15 * R, tw, tw * 0.8, TrCR, TrCG, TrCB, 1);
  TrRamp(7, 0.6);
  TrCap(bx + tw * 0.2, by, cx + tw * 0.15, cy + 0.15 * R, tw * 0.45, tw * 0.35, TrCR, TrCG, TrCB, 1);
  for kk := 0 to nf - 1 do
  begin
    k := ord[kk];
    a := ang[k];
    ln := R * (0.86 + 0.2 * TrOH(2, k, 1));
    wd := ln * 0.265;
    s := Cos(a) * (-Sin(a));
    bnd := (0.34 + 0.22 * TrOH(2, k, 2)) * (1 - 2 * Floor(TrOH(2, k, 3) * 2));
    if Cos(a) > 0.15 then
      bnd := Abs(bnd)
    else if Cos(a) < -0.15 then
      bnd := -Abs(bnd);
    if (Abs(Cos(a)) <= 0.15) and (Sin(a) > 0) then
      bnd := bnd * 0.5;
    ld := Cos(a) * TR_LX + Sin(a) * TR_LY;
    tone := 0.66 + 0.28 * ld + 0.12 * (TrOH(2, k, 4) - 0.5);
    if Sin(a) < -0.2 then
      tone := tone - 0.06;
    TrRamp(3, tone + 0.14);
    lr := TrCR;
    lg := TrCG;
    lb := TrCB;
    TrRamp(3, tone - 0.30);
    TrSoft := 1.0;
    TrLeaf(cx, cy, a, ln, wd, bnd, lr, lg, lb, TrCR, TrCG, TrCB, 0.7, 0.28, 1);
    TrSoft := 0.3;
  end;
  TrRamp(3, 0.9);
  TrDisk(cx, cy, R * 0.07, TrCR, TrCG, TrCB, 1);
  TrDisk(cx + R * 0.03, cy - R * 0.03, R * 0.035, 214, 214, 96, 1);
  TrDisk(cx - R * 0.04, cy + R * 0.02, R * 0.03, 200, 204, 84, 1);
end;

procedure TrDrawRoset(bx, by, sz: Single; pv, sd: Integer);
var
  n, k, pass: Integer;
  a0, a, ln, wd, bnd, ld, tone, cx, cy, s, sh: Single;
begin
  TrOS := sd;
  cx := bx;
  cy := by - sz * 0.3;
  n := 6 + Floor(TrOH(0, 0, 1) * 4);
  a0 := TrOH(0, 0, 2) * 6.2832;
  sh := sz * 0.34;
  if TrMode = 1 then
    TrShA := 0.85;
  for pass := 0 to 1 do
    for k := 0 to n - 1 do
    begin
      a := a0 + k * 6.2832 / n + (TrOH(1, k, 1) - 0.5) * 0.5;
      s := Sin(a);
      if (pass = 0) and (s >= 0.2) then
        Continue;
      if (pass = 1) and (s < 0.2) then
        Continue;
      ln := sz * 0.56 * (0.8 + 0.35 * TrOH(1, k, 2)) * (1 - 0.22 * TrMax(0, s));
      wd := ln * 0.27;
      bnd := (TrOH(1, k, 3) - 0.5) * 0.5;
      if s > 0 then
        bnd := bnd * 0.6 + 0.10 * Cos(a);
      if TrMode = 1 then
      begin
        TrLeaf(cx + TR_SX * sh, cy + TR_SY * sh, a, ln, wd, bnd, 0, 0, 0, 0, 0, 0, 0, 0, 1);
        Continue;
      end;
      ld := Cos(a) * TR_LX + s * TR_LY;
      tone := 0.55 + 0.30 * ld + 0.14 * (TrOH(1, k, 4) - 0.5);
      TrLeafR(cx, cy, a, ln, wd, bnd, tone, 0.6, 0, 1, 0.14, pv);
    end;
  if TrMode = 0 then
  begin
    TrRamp(pv, 0.35);
    TrDisk(cx, cy, sz * 0.06, TrCR, TrCG, TrCB, 1);
  end;
end;

procedure TrDrawTuft(bx, by, sz: Single; sd: Integer);
var
  n, k: Integer;
  a, ln, tone, sh: Single;
begin
  TrOS := sd;
  n := 3 + Floor(TrOH(0, 0, 1) * 3);
  sh := sz * 0.3;
  if TrMode = 1 then
    TrShA := 0.6;
  for k := 0 to n - 1 do
  begin
    a := -1.5708 + (TrOH(1, k, 1) - 0.5) * 1.9;
    ln := sz * (0.7 + 0.5 * TrOH(1, k, 2));
    if TrMode = 1 then
    begin
      TrLeaf(bx + TR_SX * sh, by + TR_SY * sh, a, ln, 1.1, 0.1, 0, 0, 0, 0, 0, 0, 0, 0, 1);
      Continue;
    end;
    tone := 0.45 + 0.4 * TrOH(1, k, 3) + 0.15 * (Cos(a) * TR_LX + Sin(a) * TR_LY);
    TrLeafR(bx, by, a, ln, 1.4, (TrOH(1, k, 4) - 0.5) * 0.5, tone, 0, 0, 1, 0.12, 4);
  end;
end;

procedure TrDrawFlower(x, y, r: Single; kind, sd: Integer);
var
  j: Integer;
  a, d, lr, lg, lb, dt: Single;
begin
  for j := 0 to 4 do
  begin
    a := j * 1.2566 + TrOH(sd, j, 3) * 1.0;
    dt := Cos(a) * TR_LX + Sin(a) * TR_LY;
    if kind = 0 then
    begin
      if dt > 0.3 then
      begin
        lr := 221;
        lg := 111;
        lb := 88;
      end
      else if dt < -0.3 then
      begin
        lr := 168;
        lg := 66;
        lb := 51;
      end
      else
      begin
        lr := 210;
        lg := 82;
        lb := 64;
      end;
    end
    else
    begin
      if dt > 0.3 then
      begin
        lr := 250;
        lg := 248;
        lb := 230;
      end
      else if dt < -0.3 then
      begin
        lr := 188;
        lg := 184;
        lb := 208;
      end
      else
      begin
        lr := 232;
        lg := 232;
        lb := 206;
      end;
    end;
    TrDisk(x + Cos(a) * r * 0.62, y + Sin(a) * r * 0.62, r * 0.6, lr, lg, lb, 1);
  end;
  if kind = 0 then
    TrDisk(x, y, r * 0.45, 190, 70, 56, 1)
  else
    TrDisk(x, y, r * 0.42, 226, 214, 150, 1);
end;

procedure TrDrawBush(bx, by, sz: Single; kind, sd: Integer);
var
  cx, cy, ang, rad, fx, fy, fr: Single;
  nf, k, o, j: Integer;
  fxs, fys: array[0..19] of Single;
  tmp: Single;
begin
  TrOS := sd;
  cx := bx;
  cy := by - sz * 0.75;
  if TrMode = 1 then
  begin
    TrShA := 0.9;
    TrEll(cx + TR_SX * sz * 0.55, cy + TR_SY * sz * 0.5 + sz * 0.15, sz * 1.0, sz * 0.85, 0, 0, 0, 1);
    Exit;
  end;
  if kind = 2 then
  begin
    TrCanopy(cx, cy, sz, sz * 0.27, 6, 2, 0.32, 0.02, 0);
    Exit;
  end;
  TrCanopy(cx, cy, sz, sz * 0.32, 0, 0, 0, -0.04, 0);
  nf := 9 + Floor(TrOH(0, 0, 1) * 7);
  for k := 0 to nf - 1 do
  begin
    ang := TrOH(1, k, 1) * 6.2832;
    rad := Sqrt(TrOH(1, k, 2)) * sz * 0.78;
    fxs[k] := cx + Cos(ang) * rad;
    fys[k] := cy + Sin(ang) * rad * 0.85 - sz * 0.05;
  end;
  for k := 1 to nf - 1 do
  begin
    tmp := fys[k];
    fx := fxs[k];
    j := k - 1;
    while (j >= 0) and (fys[j] > tmp) do
    begin
      fys[j + 1] := fys[j];
      fxs[j + 1] := fxs[j];
      j := j - 1;
    end;
    fys[j + 1] := tmp;
    fxs[j + 1] := fx;
  end;
  fr := 2.3 + sz * 0.11;
  for k := 0 to nf - 1 do
    TrDrawFlower(fxs[k], fys[k], fr * (0.85 + 0.3 * TrOH(1, k, 5)), kind, k + 5);
end;

function TrForestAt(wx, wy: Single): Single;
var
  qx, qy, n: Single;
begin
  qx := wx + 260 * (FBM(wx / 330 + 5.3, wy / 330 + 1.7, TrSeed + 211, 2) - 0.5);
  qy := wy + 260 * (FBM(wx / 330 + 9.1, wy / 330 + 6.2, TrSeed + 212, 2) - 0.5);
  n := FBM(qx / 760 + 13.7, qy / 760 + 4.1, TrSeed + 201, 3);
  Result := SStep(0.47, 0.53, n);
end;

procedure TrPathAt(wx, wy: Single);
var
  qx, qy, p, gx, gy, g, dpx, hw, reg, wob: Single;
begin
  qx := wx + 300 * (FBM(wx / 420 + 2.1, wy / 420 + 8.8, TrSeed + 311, 2) - 0.5);
  qy := wy + 300 * (FBM(wx / 420 + 7.7, wy / 420 + 3.4, TrSeed + 312, 2) - 0.5);
  p := FBM(qx / 1250 + 7.1, qy / 1250 + 3.3, TrSeed + 301, 2);
  gx := FBM((qx + 6) / 1250 + 7.1, qy / 1250 + 3.3, TrSeed + 301, 2) - p;
  gy := FBM(qx / 1250 + 7.1, (qy + 6) / 1250 + 3.3, TrSeed + 301, 2) - p;
  g := Sqrt(gx * gx + gy * gy) / 6;
  dpx := Abs(p - 0.5) / (g + 0.00002);
  if dpx > 3000 then
    dpx := 3000;
  reg := SStep(0.40, 0.54, FBM(wx / 2400 + 4.4, wy / 2400 + 1.9, TrSeed + 331, 2));
  wob := 0.85 + 0.35 * FBM(wx / 140 + 3.0, wy / 140 + 9.0, TrSeed + 341, 2);
  hw := 24 * wob * reg;
  if hw < 2 then
  begin
    TrPM := 0;
    TrPE := 0;
    Exit;
  end;
  TrPM := SStep(hw + 2.5, hw - 2.5, dpx);
  TrPE := SStep(hw + 70, hw + 6, dpx) * (1 - TrPM);
end;

procedure TrFieldRowJob(gy: Integer);
var
  gx: Integer;
  wx, wy: Single;
begin
  wy := (TrGY0 + gy) * 4;
  for gx := 0 to TrGW - 1 do
  begin
    wx := (TrGX0 + gx) * 4;
    TrFF[gy * TrGW + gx] := TrForestAt(wx, wy);
    TrGN[gy * TrGW + gx] := FBM(wx / 110, wy / 110, TrSeed + 101, 2);
    TrPathAt(wx, wy);
    TrPF[gy * TrGW + gx] := TrPM;
  end;
end;

procedure TrRunFields(Total: Integer);
begin
  RunRowJob(TrFieldRowJob, Total);
end;

procedure TrPebbleAt(wx, wy: Single);
var
  ci, cj, di, dj, i, j: Integer;
  px, py, rp, u, ub, bx, by, br, bh, dx, dy, z, lam, t, d, sh, nrm: Single;
begin
  ci := Floor(wx / 10);
  cj := Floor(wy / 10);
  ub := 9;
  bx := 0;
  by := 0;
  br := 4;
  bh := 0;
  for dj := -1 to 1 do
    for di := -1 to 1 do
    begin
      i := ci + di;
      j := cj + dj;
      px := (i + 0.18 + 0.64 * Hash2(i, j, TrSeed + 401)) * 10;
      py := (j + 0.18 + 0.64 * Hash2(i, j, TrSeed + 402)) * 10;
      rp := 3.6 + 2.4 * Hash2(i, j, TrSeed + 403);
      u := Sqrt(Sqr(wx - px) + Sqr(wy - py)) / rp;
      if u < ub then
      begin
        ub := u;
        bx := px;
        by := py;
        br := rp;
        bh := Hash2(i, j, TrSeed + 404);
      end;
    end;
  dx := (wx - bx) / br;
  dy := (wy - by) / br;
  if ub < 1 then
  begin
    z := Sqrt(1 - ub * ub);
    lam := dx * 0.55 - dy * 0.65 + z * 0.55;
    t := TrClamp(0.45 + 0.55 * lam + (bh - 0.5) * 0.24, 0, 1);
    TrCR := 166 + (226 - 166) * t;
    TrCG := 132 + (192 - 132) * t;
    TrCB := 74 + (132 - 74) * t;
    d := SStep(0.80, 1.0, ub) * 0.45;
    TrCR := TrCR + (146 - TrCR) * d;
    TrCG := TrCG + (116 - TrCG) * d;
    TrCB := TrCB + (60 - TrCB) * d;
  end
  else
  begin
    nrm := Sqrt(dx * dx + dy * dy) + 0.001;
    sh := (dx * TR_SX + dy * TR_SY) / nrm;
    d := (1 - SStep(1.0, 1.7, ub)) * (0.45 + 0.55 * TrMax(0, sh));
    TrCR := 150 * (1 - 0.38 * d);
    TrCG := 120 * (1 - 0.38 * d);
    TrCB := 62 * (1 - 0.34 * d);
  end;
end;

procedure TrGroundRows(ys0, ys1: Integer);
var
  xi, yi, o, ix, iy, gxw, gyw, gcx, gcy: Integer;
  wx, wy, fx, fy, f, pm, n1, dv, h, r, g, b, t, m, edge, moss, fl, pmf, pa, fr, fg, fb, w00, w10, w01, w11, k, h2: Single;
begin
  for yi := ys0 to ys1 do
  begin
    gyw := Floor(TrWY) + yi;
    gcy := Floor(gyw / 4);
    iy := gcy - TrGY0;
    fy := (gyw - gcy * 4 + 0.5) / 4;
    for xi := 0 to TrW - 1 do
    begin
      o := yi * TrW + xi;
      gxw := Floor(TrWX) + xi;
      gcx := Floor(gxw / 4);
      ix := gcx - TrGX0;
      fx := (gxw - gcx * 4 + 0.5) / 4;
      w00 := (1 - fx) * (1 - fy);
      w10 := fx * (1 - fy);
      w01 := (1 - fx) * fy;
      w11 := fx * fy;
      f := TrFF[iy * TrGW + ix] * w00 + TrFF[iy * TrGW + ix + 1] * w10 + TrFF[(iy + 1) * TrGW + ix] * w01 + TrFF[(iy + 1) * TrGW + ix + 1] * w11;
      pm := TrPF[iy * TrGW + ix] * w00 + TrPF[iy * TrGW + ix + 1] * w10 + TrPF[(iy + 1) * TrGW + ix] * w01 + TrPF[(iy + 1) * TrGW + ix + 1] * w11;
      wx := TrWX + xi + 0.5;
      wy := TrWY + yi + 0.5;

      n1 := TrGN[iy * TrGW + ix] * w00 + TrGN[iy * TrGW + ix + 1] * w10 + TrGN[(iy + 1) * TrGW + ix] * w01 + TrGN[(iy + 1) * TrGW + ix + 1] * w11;
      dv := (n1 - 0.5) * 12;
      r := 146 + dv;
      g := 105 + dv * 0.72;
      b := 51 + dv * 0.35;
      h := Hash2(Floor(wx), Floor(wy), TrSeed + 7);
      if h < 0.035 then
      begin
        r := r + 10;
        g := g + 8;
        b := b + 4;
      end
      else if h > 0.972 then
      begin
        r := r - 14;
        g := g - 11;
        b := b - 6;
      end;
      edge := SStep(0.04, 0.3, f) * (1 - SStep(0.55, 0.85, f));
      if edge > 0.01 then
      begin
        t := FBM(wx / 36, wy / 36, TrSeed + 71, 2);
        moss := SStep(0.44, 0.62, t) * edge;
        r := r + (88 - r) * moss * 0.9;
        g := g + (108 - g) * moss * 0.9;
        b := b + (44 - b) * moss * 0.9;
        h2 := Hash2(Floor(wx / 2), Floor(wy / 2), TrSeed + 9);
        if h2 < 0.07 * edge then
        begin
          r := 70;
          g := 122;
          b := 40;
        end;
      end;

      fl := SStep(0.30, 0.62, f);
      if fl > 0.005 then
      begin
        m := FBM(wx / 15, wy / 15, TrSeed + 61, 3);
        t := SStep(0.22, 0.85, m) * 3;
        if t < 1 then
        begin
          fr := 18 + 20 * t;
          fg := 86 + 8 * t;
          fb := 43 + t;
        end
        else if t < 2 then
        begin
          fr := 38 + 26 * (t - 1);
          fg := 94 + 4 * (t - 1);
          fb := 44;
        end
        else
        begin
          fr := 64 + 50 * (t - 2);
          fg := 98 + 3 * (t - 2);
          fb := 44 + 14 * (t - 2);
        end;
        if Hash2(Floor(wx), Floor(wy), TrSeed + 13) < 0.025 then
        begin
          fr := fr + 30;
          fg := fg + 30;
        end;
        r := r + (fr - r) * fl;
        g := g + (fg - g) * fl;
        b := b + (fb - b) * fl;
      end;

      if pm > 0.01 then
      begin
        pmf := pm + (FBM(wx / 9, wy / 9, TrSeed + 81, 2) - 0.5) * 0.4;
        pa := SStep(0.40, 0.56, pmf);
        if pa > 0.004 then
        begin
          TrPebbleAt(wx, wy);
          r := r + (TrCR - r) * pa;
          g := g + (TrCG - g) * pa;
          b := b + (TrCB - b) * pa;
        end;
        k := SStep(0.16, 0.34, pmf) * (1 - pa);
        if k > 0.01 then
        begin
          r := r + (100 - r) * k * 0.55;
          g := g + (138 - g) * k * 0.55;
          b := b + (44 - b) * k * 0.55;
        end;
      end;

      TrPR[o] := r;
      TrPG[o] := g;
      TrPB[o] := b;
    end;
  end;
end;

var
  TrPickX, TrPickY: Single;
  TrPickSeed: Integer;

procedure TrAdd(kind, sub: Integer; x, y, sz: Single; sd: Integer);
begin
  if TrN >= TR_MAXOBJ then
    Exit;
  TrObj[TrN].Kind := kind;
  TrObj[TrN].Sub := sub;
  TrObj[TrN].Seed := sd;
  TrObj[TrN].X := x;
  TrObj[TrN].Y := y;
  TrObj[TrN].Sz := sz;
  TrObj[TrN].Z := y;
  TrOrd[TrN] := TrN;
  TrN := TrN + 1;
end;

procedure TrGenTrees;
var
  ci, cj, ci0, ci1, cj0, cj1, kind, pv: Integer;
  px, py, f, p, r0, R, hv: Single;
  blk: Boolean;
begin
  ci0 := Floor((TrWX - 130) / TR_CELL_TREE);
  ci1 := Floor((TrWX + TrW + 130) / TR_CELL_TREE);
  cj0 := Floor((TrWY - 90) / TR_CELL_TREE);
  cj1 := Floor((TrWY + TrH + 250) / TR_CELL_TREE);
  for cj := cj0 to cj1 do
    for ci := ci0 to ci1 do
    begin
      px := (ci + 0.5 + (TrHsh(ci, cj, 2) - 0.5) * 0.8) * TR_CELL_TREE;
      py := (cj + 0.5 + (TrHsh(ci, cj, 3) - 0.5) * 0.8) * TR_CELL_TREE;
      f := TrForestAt(px, py);
      p := SStep(0.26, 0.55, f) * 1.0;
      if TrHsh(ci, cj, 1) >= p then
        Continue;
      r0 := TrHsh(ci, cj, 5);
      if r0 < 0.34 then
        R := 68 + 26 * TrHsh(ci, cj, 6)
      else if r0 < 0.76 then
        R := 50 + 16 * TrHsh(ci, cj, 6)
      else
        R := 34 + 10 * TrHsh(ci, cj, 6);
      R := R * (0.82 + 0.18 * SStep(0.4, 0.85, f));
      blk := False;
      TrPathAt(px, py);
      if TrPM > 0.1 then
        blk := True;
      TrPathAt(px, py - 1.3 * R);
      if TrPM > 0.1 then
        blk := True;
      TrPathAt(px - 0.7 * R, py - 1.3 * R);
      if TrPM > 0.1 then
        blk := True;
      TrPathAt(px + 0.7 * R, py - 1.3 * R);
      if TrPM > 0.1 then
        blk := True;
      TrPathAt(px, py - 2.0 * R);
      if TrPM > 0.1 then
        blk := True;
      if blk then
        Continue;
      kind := 0;
      if (TrHsh(ci, cj, 7) < 0.2) and (f > 0.3) then
        kind := 1;
      hv := TrHsh(ci, cj, 8);
      if hv < 0.6 then
        pv := 0
      else if hv < 0.88 then
        pv := 1
      else
        pv := 2;
      TrAdd(kind, pv, px, py, R, Floor(TrHsh(ci, cj, 9) * 1000000));
    end;
end;

procedure TrGenUnder;
var
  ci, cj, ci0, ci1, cj0, cj1, sd: Integer;
  px, py, f, p, belt, pe, k2, hb: Single;
begin
  ci0 := Floor((TrWX - 100) / TR_CELL_UNDER);
  ci1 := Floor((TrWX + TrW + 100) / TR_CELL_UNDER);
  cj0 := Floor((TrWY - 60) / TR_CELL_UNDER);
  cj1 := Floor((TrWY + TrH + 120) / TR_CELL_UNDER);
  for cj := cj0 to cj1 do
    for ci := ci0 to ci1 do
    begin
      px := (ci + 0.5 + (TrHsh(ci, cj, 42) - 0.5) * 0.85) * TR_CELL_UNDER;
      py := (cj + 0.5 + (TrHsh(ci, cj, 43) - 0.5) * 0.85) * TR_CELL_UNDER;
      f := TrForestAt(px, py);
      TrPathAt(px, py);
      if TrPM > 0.22 then
        Continue;
      belt := 0.36 * 4 * f * (1 - f);
      pe := 0.85 * TrPE;
      p := 0.66 * SStep(0.22, 0.6, f);
      if belt > p then
        p := belt;
      if pe > p then
        p := pe;
      if TrHsh(ci, cj, 41) >= p then
        Continue;
      sd := Floor(TrHsh(ci, cj, 44) * 1000000);
      k2 := TrHsh(ci, cj, 45);
      if k2 < 0.55 then
        TrAdd(2, 4, px, py, 22 + 12 * TrHsh(ci, cj, 46), sd)
      else if k2 < 0.66 then
      begin
        if f > 0.4 then
          TrAdd(1, 0, px, py, 36 + 14 * TrHsh(ci, cj, 46), sd)
        else
          TrAdd(2, 4, px, py, 22 + 12 * TrHsh(ci, cj, 46), sd);
      end
      else if k2 < 0.78 then
      begin
        hb := TrHsh(ci, cj, 47);
        if hb < 0.4 then
          TrAdd(4, 0, px, py, 17 + 7 * TrHsh(ci, cj, 46), sd)
        else if hb < 0.7 then
          TrAdd(4, 1, px, py, 17 + 7 * TrHsh(ci, cj, 46), sd)
        else
          TrAdd(4, 2, px, py, 26 + 5 * TrHsh(ci, cj, 46), sd);
      end
      else
        TrAdd(3, 0, px, py, 8 + 4 * TrHsh(ci, cj, 46), sd);
    end;
end;

procedure TrGenOpen;
var
  ci, cj, ci0, ci1, cj0, cj1, k, n, sd, ii: Integer;
  cs, px, py, f, p, rr, a, d, x2, y2, c, hb: Single;
begin
  cs := TR_CELL_OPEN;
  ci0 := Floor((TrWX - 130) / cs);
  ci1 := Floor((TrWX + TrW + 130) / cs);
  cj0 := Floor((TrWY - 90) / cs);
  cj1 := Floor((TrWY + TrH + 120) / cs);
  for cj := cj0 to cj1 do
    for ci := ci0 to ci1 do
    begin
      px := (ci + 0.5 + (TrHsh(ci, cj, 21) - 0.5) * 0.7) * cs;
      py := (cj + 0.5 + (TrHsh(ci, cj, 22) - 0.5) * 0.7) * cs;
      f := TrForestAt(px, py);
      p := 0.5 * (1 - SStep(0.10, 0.38, f));
      if TrHsh(ci, cj, 20) >= p then
        Continue;
      TrPathAt(px, py);
      if TrPM > 0.2 then
        Continue;
      sd := Floor(TrHsh(ci, cj, 23) * 1000000);
      c := TrHsh(ci, cj, 24);
      ii := (ci * 31 + cj * 17) and 1023;
      if c < 0.55 then
      begin
        rr := 13 + 11 * TrHsh(ci, cj, 25);
        TrAdd(5, 9 + Floor(TrHsh(ci, cj, 26) * 5), px, py, rr, sd);
        n := 3 + Floor(TrHsh(ci, cj, 27) * 4);
        for k := 0 to n - 1 do
        begin
          a := TrHsh(ii * 16 + k, cj, 30) * 6.2832;
          d := rr * (1.15 + 0.9 * TrHsh(ii * 16 + k, cj, 31)) + 8;
          x2 := px + Cos(a) * d;
          y2 := py + Sin(a) * d * 0.8 + rr * 0.3;
          if TrHsh(ii * 16 + k, cj, 32) < 0.66 then
            TrAdd(2, 4, x2, y2, 18 + 14 * TrHsh(ii * 16 + k, cj, 33), sd + k * 7 + 1)
          else
            TrAdd(3, 0, x2, y2, 8 + 5 * TrHsh(ii * 16 + k, cj, 33), sd + k * 7 + 1);
        end;
        n := 1 + Floor(TrHsh(ci, cj, 28) * 3);
        for k := 0 to n - 1 do
        begin
          a := TrHsh(ii * 16 + k, cj, 34) * 6.2832;
          d := rr * (1.3 + 1.2 * TrHsh(ii * 16 + k, cj, 35));
          TrAdd(5, 4, px + Cos(a) * d, py + Sin(a) * d * 0.8 + 3, 2 + 2 * TrHsh(ii * 16 + k, cj, 36), sd + k * 11 + 3);
        end;
      end
      else if c < 0.80 then
      begin
        n := 3 + Floor(TrHsh(ci, cj, 27) * 3);
        for k := 0 to n - 1 do
        begin
          a := TrHsh(ii * 16 + k, cj, 30) * 6.2832;
          d := 6 + 26 * TrHsh(ii * 16 + k, cj, 31);
          x2 := px + Cos(a) * d;
          y2 := py + Sin(a) * d * 0.8;
          TrAdd(2, 4, x2, y2, 20 + 14 * TrHsh(ii * 16 + k, cj, 33), sd + k * 7 + 1);
        end;
        n := 2 + Floor(TrHsh(ci, cj, 28) * 3);
        for k := 0 to n - 1 do
        begin
          a := TrHsh(ii * 16 + k, cj, 34) * 6.2832;
          d := 12 + 30 * TrHsh(ii * 16 + k, cj, 35);
          TrAdd(3, 0, px + Cos(a) * d, py + Sin(a) * d * 0.8, 8 + 5 * TrHsh(ii * 16 + k, cj, 36), sd + k * 11 + 3);
        end;
      end
      else if c < 0.92 then
      begin
        hb := TrHsh(ci, cj, 29);
        if hb < 0.6 then
          TrAdd(4, 1, px, py, 16 + 6 * TrHsh(ci, cj, 25), sd)
        else
          TrAdd(4, 0, px, py, 16 + 6 * TrHsh(ci, cj, 25), sd);
        for k := 0 to 1 do
        begin
          a := TrHsh(ii * 16 + k, cj, 30) * 6.2832;
          TrAdd(2, 4, px + Cos(a) * 30, py + Sin(a) * 24, 18 + 10 * TrHsh(ii * 16 + k, cj, 33), sd + k * 7 + 1);
        end;
      end
      else
      begin
        TrAdd(2, 4, px, py, 24 + 8 * TrHsh(ci, cj, 25), sd);
        TrAdd(2, 4, px + 24, py + 10, 20 + 8 * TrHsh(ci, cj, 26), sd + 5);
        TrAdd(3, 0, px - 20, py + 8, 10, sd + 9);
      end;
    end;
end;

procedure TrGenRockF;
var
  ci, cj, ci0, ci1, cj0, cj1, k, sd: Integer;
  cs, px, py, f, rr, a, d: Single;
begin
  cs := TR_CELL_ROCKF;
  ci0 := Floor((TrWX - 100) / cs);
  ci1 := Floor((TrWX + TrW + 100) / cs);
  cj0 := Floor((TrWY - 60) / cs);
  cj1 := Floor((TrWY + TrH + 100) / cs);
  for cj := cj0 to cj1 do
    for ci := ci0 to ci1 do
    begin
      px := (ci + 0.5 + (TrHsh(ci, cj, 61) - 0.5) * 0.7) * cs;
      py := (cj + 0.5 + (TrHsh(ci, cj, 62) - 0.5) * 0.7) * cs;
      f := TrForestAt(px, py);
      if TrHsh(ci, cj, 60) >= 0.32 * SStep(0.4, 0.8, f) then
        Continue;
      TrPathAt(px, py);
      if TrPM > 0.1 then
        Continue;
      sd := Floor(TrHsh(ci, cj, 63) * 1000000);
      rr := 18 + 10 * TrHsh(ci, cj, 64);
      TrAdd(5, 11, px, py, rr, sd);
      for k := 0 to 2 do
      begin
        a := TrHsh(ci * 4 + k, cj, 65) * 6.2832;
        d := rr * (1.1 + 0.8 * TrHsh(ci * 4 + k, cj, 66)) + 6;
        if k = 0 then
          TrAdd(3, 0, px + Cos(a) * d, py + Sin(a) * d * 0.8 + 4, 10, sd + k + 1)
        else
          TrAdd(2, 4, px + Cos(a) * d, py + Sin(a) * d * 0.8 + 4, 20 + 8 * TrHsh(ci * 4 + k, cj, 67), sd + k + 1);
      end;
    end;
end;

procedure TrGenTuft;
var
  ci, cj, ci0, ci1, cj0, cj1, sd: Integer;
  cs, px, py, f, p: Single;
begin
  cs := TR_CELL_TUFT;
  ci0 := Floor((TrWX - 60) / cs);
  ci1 := Floor((TrWX + TrW + 60) / cs);
  cj0 := Floor((TrWY - 40) / cs);
  cj1 := Floor((TrWY + TrH + 60) / cs);
  for cj := cj0 to cj1 do
    for ci := ci0 to ci1 do
    begin
      px := (ci + 0.5 + (TrHsh(ci, cj, 72) - 0.5) * 0.9) * cs;
      py := (cj + 0.5 + (TrHsh(ci, cj, 73) - 0.5) * 0.9) * cs;
      f := TrForestAt(px, py);
      p := 0.13 * (1 - SStep(0.05, 0.3, f));
      if TrHsh(ci, cj, 71) >= p then
        Continue;
      TrPathAt(px, py);
      if TrPM > 0.2 then
        Continue;
      sd := Floor(TrHsh(ci, cj, 74) * 1000000);
      if TrHsh(ci, cj, 75) < 0.5 then
        TrAdd(3, 0, px, py, 8 + 6 * TrHsh(ci, cj, 76), sd)
      else
        TrAdd(2, 4, px, py, 13 + 8 * TrHsh(ci, cj, 76), sd);
    end;
end;

procedure TrGenCob;
var
  ci, cj, ci0, ci1, cj0, cj1: Integer;
  cs, px, py: Single;
begin
  cs := TR_CELL_COB;
  ci0 := Floor((TrWX - 20) / cs);
  ci1 := Floor((TrWX + TrW + 20) / cs);
  cj0 := Floor((TrWY - 20) / cs);
  cj1 := Floor((TrWY + TrH + 20) / cs);
  for cj := cj0 to cj1 do
    for ci := ci0 to ci1 do
    begin
      px := (ci + 0.5 + (TrHsh(ci, cj, 82) - 0.5) * 0.9) * cs;
      py := (cj + 0.5 + (TrHsh(ci, cj, 83) - 0.5) * 0.9) * cs;
      if TrHsh(ci, cj, 81) >= 0.22 then
        Continue;
      TrPathAt(px, py);
      if TrPM < 0.7 then
        Continue;
      TrAdd(5, 4, px, py, 3.5 + 3 * TrHsh(ci, cj, 84), Floor(TrHsh(ci, cj, 85) * 1000000));
    end;
end;

procedure TrSortObjs;
var
  gap, i, j, tmp: Integer;
begin
  gap := TrN div 2;
  while gap > 0 do
  begin
    for i := gap to TrN - 1 do
    begin
      tmp := TrOrd[i];
      j := i;
      while (j >= gap) and (TrObj[TrOrd[j - gap]].Z > TrObj[tmp].Z) do
      begin
        TrOrd[j] := TrOrd[j - gap];
        j := j - gap;
      end;
      TrOrd[j] := tmp;
    end;
    gap := gap div 2;
  end;
end;

procedure TrDrawAll(md: Integer);
var
  i, k, kd: Integer;
  sx, sy: Single;
begin
  TrMode := md;
  for i := 0 to TrN - 1 do
  begin
    k := TrOrd[i];
    sx := TrObj[k].X - TrWX;
    sy := TrObj[k].Y - TrWY;
    if (sy + 0.6 * TrObj[k].Sz + 16 < TrY0) or (sy - 2.6 * TrObj[k].Sz - 12 > TrY1) then
      Continue;
    kd := TrObj[k].Kind;
    if kd = 0 then
      TrDrawTree(sx, sy, TrObj[k].Sz, TrObj[k].Sub, TrObj[k].Seed)
    else if kd = 1 then
      TrDrawPalm(sx, sy, TrObj[k].Sz, TrObj[k].Seed)
    else if kd = 2 then
      TrDrawRoset(sx, sy, TrObj[k].Sz, TrObj[k].Sub, TrObj[k].Seed)
    else if kd = 3 then
      TrDrawTuft(sx, sy, TrObj[k].Sz, TrObj[k].Seed)
    else if kd = 4 then
      TrDrawBush(sx, sy, TrObj[k].Sz, TrObj[k].Sub, TrObj[k].Seed)
    else
      TrDrawRock(sx, sy, TrObj[k].Sz, TrObj[k].Sub, TrObj[k].Seed);
  end;
  TrMode := 0;
end;

procedure TrApplyShadowRows(ys0, ys1: Integer);
var
  o: Integer;
  sm: Single;
begin
  for o := ys0 * TrW to (ys1 + 1) * TrW - 1 do
  begin
    sm := TrSH[o];
    if sm > 0.003 then
    begin
      TrPR[o] := TrPR[o] * (1 - sm * 0.34);
      TrPG[o] := TrPG[o] * (1 - sm * 0.27);
      TrPB[o] := TrPB[o] * (1 - sm * 0.19);
    end;
  end;
end;

procedure TrBandJob(bi: Integer);
var
  y0, y1, o: Integer;
begin
  y0 := bi * TR_BAND;
  y1 := y0 + TR_BAND - 1;
  if y1 > TrH - 1 then
    y1 := TrH - 1;
  TrY0 := y0;
  TrY1 := y1;
  TrSoft := 0.3;
  TrMode := 0;
  TrShA := 1;
  for o := y0 * TrW to (y1 + 1) * TrW - 1 do
    TrSH[o] := 0;
  TrGroundRows(y0, y1);
  TrDrawAll(1);
  TrApplyShadowRows(y0, y1);
  TrDrawAll(0);
end;

procedure TrRunBands(Total: Integer);
begin
  RunRowJob(TrBandJob, Total);
end;

procedure TrRenderWorld(W, H: Integer; WX, WY: Single; ASeed: Integer);
begin
  TrW := W;
  TrH := H;
  TrWX := WX;
  TrWY := WY;
  TrSeed := ASeed;
  TrY0 := 0;
  TrY1 := H - 1;
  TrSoft := 0.3;
  SetLength(TrPR, W * H);
  SetLength(TrPG, W * H);
  SetLength(TrPB, W * H);
  SetLength(TrSH, W * H);
  TrInitPal;
  TrGX0 := Floor(WX / 4);
  TrGY0 := Floor(WY / 4);
  TrGW := W div 4 + 4;
  TrGH := H div 4 + 4;
  SetLength(TrFF, TrGW * TrGH);
  SetLength(TrPF, TrGW * TrGH);
  SetLength(TrGN, TrGW * TrGH);
  TrRunFields(TrGH);
  TrN := 0;
  TrGenTrees;
  TrGenUnder;
  TrGenOpen;
  TrGenRockF;
  TrGenTuft;
  TrGenCob;
  TrSortObjs;
  TrRunBands((H + TR_BAND - 1) div TR_BAND);
end;

procedure TrPickWindow(W, H: Integer);
var
  tries, i, j, np: Integer;
  ff, pf: Single;
  ok: Boolean;
begin
  for tries := 1 to 60 do
  begin
    TrSeed := Random(900000) + 1;
    TrPickX := Random(200000) + 5000;
    TrPickY := Random(200000) + 5000;
    ff := 0;
    for j := 0 to 9 do
      for i := 0 to 13 do
        ff := ff + TrForestAt(TrPickX + (i + 0.5) * W / 14, TrPickY + (j + 0.5) * H / 10);
    ff := ff / 140;
    np := 0;
    for j := 0 to 17 do
      for i := 0 to 23 do
      begin
        TrPathAt(TrPickX + (i + 0.5) * W / 24, TrPickY + (j + 0.5) * H / 18);
        if TrPM > 0.5 then
          np := np + 1;
      end;
    pf := np / 432;
    ok := (ff >= 0.26) and (ff <= 0.62) and ((pf > 0.02) or (Random < 0.3) or (tries > 40));
    if ok then
      Break;
  end;
  TrPickSeed := TrSeed;
end;

type
  TwCanvas = TBitmap32;

var
  TwSeed: Integer;
  TrBmpBits: PCardinalArray;
  TrDstOff, TrDstStride: Integer;

procedure TrFlushRowJob(y: Integer);
var
  x, o, d: Integer;
begin
  o := y * TrW;
  d := TrDstOff + y * TrDstStride;
  for x := 0 to TrW - 1 do
    TrBmpBits^[d + x] := Color32(Clamp255(TrPR[o + x]), Clamp255(TrPG[o + x]), Clamp255(TrPB[o + x]), 255);
end;

function TwNewCanvas: TwCanvas;
begin
  Result := TBitmap32.Create;
end;

procedure TwFreeCanvas(c: TwCanvas);
begin
  c.Free;
end;

procedure TwSizeCanvas(c: TwCanvas; w, h: Integer);
begin
  c.SetSize(w, h);
end;

function TwCW(c: TwCanvas): Integer;
begin
  Result := c.Width;
end;

function TwCH(c: TwCanvas): Integer;
begin
  Result := c.Height;
end;

procedure TwCopyRect(src: TwCanvas; sx, sy, w, h: Integer; dst: TwCanvas; dx, dy: Integer);
var
  y: Integer;
  ps, pd: PCardinalArray;
begin
  ps := PCardinalArray(@src.Bits[0]);
  pd := PCardinalArray(@dst.Bits[0]);
  for y := 0 to h - 1 do
    Move(ps^[(sy + y) * src.Width + sx], pd^[(dy + y) * dst.Width + dx], w * 4);
end;

procedure TwRenderRegion(dst: TwCanvas; dx, dy, RW, RH, WX, WY: Integer);
begin
  TrRenderWorld(RW, RH, WX, WY, TwSeed);
  TrBmpBits := PCardinalArray(@dst.Bits[0]);
  TrDstOff := dy * dst.Width + dx;
  TrDstStride := dst.Width;
  RunRowJob(TrFlushRowJob, RH);
end;

function TwPixEq(a: TwCanvas; ax, ay: Integer; b: TwCanvas; bx, by: Integer): Boolean;
begin
  Result := PCardinalArray(@a.Bits[0])^[ay * a.Width + ax] = PCardinalArray(@b.Bits[0])^[by * b.Width + bx];
end;

procedure TwOutput(c: TwCanvas);
begin
  SaveToJPEG(c, ExtractFilePath(ParamStr(0)) + 'BG.jpg', 85);
end;

procedure TerraRenderToBitmap(bmp: TBitmap32; WX, WY, ASeed: Integer);
begin
  TwSeed := ASeed;
  TwRenderRegion(bmp, 0, 0, bmp.Width, bmp.Height, WX, WY);
end;

const
  TR_PAD = 192;
  TR_LEAD = 48;
  TR_TILE = 1024;
  TR_SAVE_MAXPIX = 25000000;

var
  TwBuf, TwTmp, TwView: TwCanvas;
  TwHave, TwActive, TwDrag, TwCropped: Boolean;
  TwBX, TwBY, TwBW, TwBH, TwVX, TwVY, TwVW, TwVH: Integer;
  TwMinX, TwMinY, TwMaxX, TwMaxY: Integer;
  TwDX, TwDY, TwDVX, TwDVY, TwSvW, TwSvH, TwSvX, TwSvY, TwSaveMax: Integer;

function TwMinI(a, b: Integer): Integer;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function TwMaxI(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

procedure TwGenTo(nbx, nby, wx, wy, w, h: Integer);
begin
  if (w > 0) and (h > 0) then
    TwRenderRegion(TwTmp, wx - nbx, wy - nby, w, h, wx, wy);
end;

procedure TwMoveBuffer(nbx, nby: Integer);
var
  ox0, oy0, ox1, oy1: Integer;
  sw: TwCanvas;
begin
  if (nbx = TwBX) and (nby = TwBY) then
    Exit;
  ox0 := TwMaxI(TwBX, nbx);
  oy0 := TwMaxI(TwBY, nby);
  ox1 := TwMinI(TwBX + TwBW, nbx + TwBW);
  oy1 := TwMinI(TwBY + TwBH, nby + TwBH);
  if (ox1 > ox0) and (oy1 > oy0) then
  begin
    TwCopyRect(TwBuf, ox0 - TwBX, oy0 - TwBY, ox1 - ox0, oy1 - oy0, TwTmp, ox0 - nbx, oy0 - nby);
    TwGenTo(nbx, nby, nbx, nby, TwBW, oy0 - nby);
    TwGenTo(nbx, nby, nbx, oy1, TwBW, nby + TwBH - oy1);
    TwGenTo(nbx, nby, nbx, oy0, ox0 - nbx, oy1 - oy0);
    TwGenTo(nbx, nby, ox1, oy0, nbx + TwBW - ox1, oy1 - oy0);
  end
  else
    TwGenTo(nbx, nby, nbx, nby, TwBW, TwBH);
  sw := TwBuf;
  TwBuf := TwTmp;
  TwTmp := sw;
  TwBX := nbx;
  TwBY := nby;
end;

procedure TwShowView;
begin
  TwCopyRect(TwBuf, TwVX - TwBX, TwVY - TwBY, TwVW, TwVH, TwView, 0, 0);
end;

procedure TwStartWorld(v: TwCanvas; W, H: Integer);
begin
  if not TwHave then
  begin
    TwBuf := TwNewCanvas;
    TwTmp := TwNewCanvas;
    TwHave := True;
  end;
  TwView := v;
  TwVW := W;
  TwVH := H;
  TrPickWindow(W, H);
  TwSeed := TrPickSeed;
  TwVX := Round(TrPickX);
  TwVY := Round(TrPickY);
  TwBW := W + 2 * TR_PAD;
  TwBH := H + 2 * TR_PAD;
  TwSizeCanvas(TwBuf, TwBW, TwBH);
  TwSizeCanvas(TwTmp, TwBW, TwBH);
  TwBX := TwVX - TR_PAD;
  TwBY := TwVY - TR_PAD;
  TwRenderRegion(TwBuf, 0, 0, TwBW, TwBH, TwBX, TwBY);
  TwSizeCanvas(TwView, W, H);
  TwShowView;
  TwMinX := TwVX;
  TwMinY := TwVY;
  TwMaxX := TwVX + W;
  TwMaxY := TwVY + H;
  TwSaveMax := TR_SAVE_MAXPIX;
  TwActive := True;
  TwDrag := False;
end;

procedure TwSetView(nvx, nvy: Integer);
var
  nbx, nby: Integer;
begin
  TwVX := nvx;
  TwVY := nvy;
  if TwVX < TwMinX then
    TwMinX := TwVX;
  if TwVY < TwMinY then
    TwMinY := TwVY;
  if TwVX + TwVW > TwMaxX then
    TwMaxX := TwVX + TwVW;
  if TwVY + TwVH > TwMaxY then
    TwMaxY := TwVY + TwVH;
  nbx := TwBX;
  nby := TwBY;
  if (TwVX < TwBX + TR_LEAD) or (TwVX + TwVW > TwBX + TwBW - TR_LEAD) then
    nbx := TwVX - TR_PAD;
  if (TwVY < TwBY + TR_LEAD) or (TwVY + TwVH > TwBY + TwBH - TR_LEAD) then
    nby := TwVY - TR_PAD;
  if (nbx <> TwBX) or (nby <> TwBY) then
    TwMoveBuffer(nbx, nby);
  TwShowView;
end;

function TwViewIsCurrent: Boolean;
var
  i, x, y: Integer;
begin
  Result := False;
  if not TwActive then
    Exit;
  if (TwCW(TwView) <> TwVW) or (TwCH(TwView) <> TwVH) then
    Exit;
  for i := 0 to 47 do
  begin
    x := (i * 7919 + 13) mod TwVW;
    y := (i * 104729 + 7) mod TwVH;
    if not TwPixEq(TwView, x, y, TwBuf, TwVX - TwBX + x, TwVY - TwBY + y) then
      Exit;
  end;
  Result := True;
end;

procedure TwBeginDrag(x, y: Integer);
begin
  if TwViewIsCurrent then
  begin
    TwDrag := True;
    TwDX := x;
    TwDY := y;
    TwDVX := TwVX;
    TwDVY := TwVY;
  end;
end;

procedure TwDragTo(x, y: Integer);
var
  nvx, nvy: Integer;
begin
  if not TwDrag then
    Exit;
  nvx := TwDVX - (x - TwDX);
  nvy := TwDVY - (y - TwDY);
  if (nvx <> TwVX) or (nvy <> TwVY) then
    TwSetView(nvx, nvy);
end;

procedure TwEndDrag;
var
  nbx, nby: Integer;
begin
  TwDrag := False;
  nbx := TwBX;
  nby := TwBY;
  if Abs(TwBX - (TwVX - TR_PAD)) > 32 then
    nbx := TwVX - TR_PAD;
  if Abs(TwBY - (TwVY - TR_PAD)) > 32 then
    nby := TwVY - TR_PAD;
  if (nbx <> TwBX) or (nby <> TwBY) then
    TwMoveBuffer(nbx, nby);
end;

procedure TwSaveAll;
var
  x0, y0, w, h, tx, ty, tw, th, nw, nh, cx, cy: Integer;
  f: Single;
  full: TwCanvas;
begin
  TwCropped := False;
  x0 := TwMinX;
  y0 := TwMinY;
  w := TwMaxX - TwMinX;
  h := TwMaxY - TwMinY;
  if Int64(w) * h > TwSaveMax then
  begin
    f := Sqrt(TwSaveMax / (Int64(w) * h));
    nw := TwMaxI(Trunc(w * f), TwMinI(w, TwVW));
    nh := TwMaxI(Trunc(h * f), TwMinI(h, TwVH));
    cx := TwVX + TwVW div 2;
    cy := TwVY + TwVH div 2;
    x0 := cx - nw div 2;
    if x0 + nw > TwMaxX then
      x0 := TwMaxX - nw;
    if x0 < TwMinX then
      x0 := TwMinX;
    y0 := cy - nh div 2;
    if y0 + nh > TwMaxY then
      y0 := TwMaxY - nh;
    if y0 < TwMinY then
      y0 := TwMinY;
    w := nw;
    h := nh;
    TwCropped := True;
  end;
  TwSvW := w;
  TwSvH := h;
  TwSvX := x0;
  TwSvY := y0;
  full := TwNewCanvas;
  TwSizeCanvas(full, w, h);
  ty := 0;
  while ty < h do
  begin
    th := TwMinI(TR_TILE, h - ty);
    tx := 0;
    while tx < w do
    begin
      tw := TwMinI(TR_TILE, w - tx);
      TwRenderRegion(full, tx, ty, tw, th, x0 + tx, y0 + ty);
      tx := tx + tw;
    end;
    ty := ty + th;
  end;
  TwOutput(full);
  TwFreeCanvas(full);
end;

type
  TTerraHost = class
  private
    FForm: TComponent;
    function GetComponentCount: Integer;
    function GetComponent(Index: Integer): TComponent;
  public
    Image321: TImage32;
    function FindComponent(const AName: string): TComponent;
    property ComponentCount: Integer read GetComponentCount;
    property Components[Index: Integer]: TComponent read GetComponent;
    procedure Image321MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image321MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure Image321MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure ButtonSaveClick(Sender: TObject);
    procedure TerraStart(AW, AH: Integer);
  end;

var
  TerraHostObj: TTerraHost;

function TTerraHost.FindComponent(const AName: string): TComponent;
begin
  if FForm = nil then
    Result := nil
  else
    Result := FForm.FindComponent(AName);
end;

function TTerraHost.GetComponentCount: Integer;
begin
  if FForm = nil then
    Result := 0
  else
    Result := FForm.ComponentCount;
end;

function TTerraHost.GetComponent(Index: Integer): TComponent;
begin
  Result := FForm.Components[Index];
end;

function TerraHost(Image321: TImage32): TTerraHost;
begin
  if TerraHostObj = nil then
    TerraHostObj := TTerraHost.Create;
  TerraHostObj.Image321 := Image321;
  TerraHostObj.FForm := Image321.Owner;
  Result := TerraHostObj;
end;

procedure TTerraHost.Image321MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if Button = mbLeft then
    TwBeginDrag(X, Y);
end;

procedure TTerraHost.Image321MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if not TwDrag then
    Exit;
  if not (ssLeft in Shift) then
  begin
    TwEndDrag;
    Image321.Invalidate;
    Exit;
  end;
  TwDragTo(X, Y);
  Image321.Invalidate;
end;

procedure TTerraHost.Image321MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  if not TwDrag then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    TwEndDrag;
  finally
    Screen.Cursor := crDefault;
  end;
  Image321.Invalidate;
end;

procedure TTerraHost.ButtonSaveClick(Sender: TObject);
begin
  if not TwViewIsCurrent then
  begin
    SaveToJPEG(Image321.Bitmap, ExtractFilePath(ParamStr(0)) + 'BG.jpg', 100);
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  try
    TwSaveAll;
  finally
    Screen.Cursor := crDefault;
  end;
  if TwCropped then
    ShowMessage('Area yang sudah digambar terlalu luas untuk satu file JPG. Yang disimpan adalah bagian terdekat dari posisi sekarang (' + IntToStr(TwSvW) + ' x ' + IntToStr(TwSvH) + ' px).');
end;

procedure TTerraHost.TerraStart(AW, AH: Integer);
var
  i: Integer;
  c: TComponent;
begin
  Screen.Cursor := crHourGlass;
  try
    TwStartWorld(Image321.Bitmap, AW, AH);
  finally
    Screen.Cursor := crDefault;
  end;
  Image321.OnMouseDown := Image321MouseDown;
  Image321.OnMouseMove := Image321MouseMove;
  Image321.OnMouseUp := Image321MouseUp;
  Image321.Cursor := crHandPoint;
  c := FindComponent('ButtonSave');
  if (c <> nil) and (c is TButton) then
    TButton(c).OnClick := ButtonSaveClick
  else
    for i := 0 to ComponentCount - 1 do
      if (Components[i] is TButton) and (Pos('SAVE', UpperCase(TButton(Components[i]).Caption)) > 0) then
        TButton(Components[i]).OnClick := ButtonSaveClick;
  SaveToJPEG(Image321.Bitmap, ExtractFilePath(ParamStr(0)) + 'BG.jpg', 85);
  Image321.Invalidate;
end;

procedure BikinTanahLumut(Image321: TImage32; AW, AH: Integer); overload;
const
  ISO = 1.7;
  MossLevel = 0.52;
  BlurRetak = 3;   OpRetak = 0.3;
  BlurMuda = 6;    OpMuda = 0.5;
  BlurTerang = 4;  OpTerang = 0.3;
  OpTua = 0.3;
  MAX_THREADS = 64;
var
  bmp: TBitmap32;
  W, H, i, seed, tries: Integer;
  tx, ty: Single;
  bufT, mC, mM, mT, mD: TFloatBuf;
  Threads1: array[0..MAX_THREADS - 1] of TMossPass1Thread;
  Threads3: array[0..MAX_THREADS - 1] of TMossPass3Thread;
  h1: array[0..MAX_THREADS - 1] of THandle;
  h3: array[0..MAX_THREADS - 1] of THandle;
  THREAD_COUNT, tIdx, totalTufts, nTufts, kCheck: Integer;
  tuftPts: array of TPoint;
  tooClose: Boolean;
begin
  Randomize;
  if Random >= TR_OLD_MOSS_CHANCE then
  begin
    TerraHost(Image321).TerraStart(AW, AH);
    Exit;
  end;
  Randomize;
  seed := Random(100000);
  W := AW;
  H := AH;
  bmp := Image321.Bitmap;
  bmp.SetSize(W, H);
  bmp.DrawMode := dmOpaque;
  SetLength(bufT, W * H);
  SetLength(mC, W * H);
  SetLength(mM, W * H);
  SetLength(mT, W * H);
  SetLength(mD, W * H);

  THREAD_COUNT := CoreCount;

  MossRowNext := 0;
  for tIdx := 0 to THREAD_COUNT - 1 do
  begin
    Threads1[tIdx] := TMossPass1Thread.Create(0, H - 1, W, seed, ISO, MossLevel, @bufT[0], @mC[0], @mM[0], @mT[0], @mD[0]);
    h1[tIdx] := Threads1[tIdx].Handle;
  end;
  WaitForMultipleObjects(THREAD_COUNT, @h1[0], True, INFINITE);
  for tIdx := 0 to THREAD_COUNT - 1 do
    Threads1[tIdx].Free;

  BlurBuf(mC, W, H, BlurRetak);
  BlurBuf(mM, W, H, BlurMuda);
  BlurBuf(mT, W, H, BlurTerang);

  MossRowNext := 0;
  for tIdx := 0 to THREAD_COUNT - 1 do
  begin
    Threads3[tIdx] := TMossPass3Thread.Create(0, H - 1, W, seed, ISO, MossLevel, OpRetak, OpMuda, OpTerang, OpTua, @bufT[0], @mC[0], @mM[0], @mT[0], @mD[0], @bmp.Bits[0]);
    h3[tIdx] := Threads3[tIdx].Handle;
  end;
  WaitForMultipleObjects(THREAD_COUNT, @h3[0], True, INFINITE);
  for tIdx := 0 to THREAD_COUNT - 1 do
    Threads3[tIdx].Free;

  DrawDirtOverlay(bmp, seed);

  for i := 1 to W * H div 25000 do
    DrawPebble(bmp, 10 + Random * (W - 20), 10 + Random * (H - 20), (1.2 + Random * 1.6) * 4, 196, 186, 166);

  totalTufts := W * H div 2000;
  SetLength(tuftPts, totalTufts);
  nTufts := 0;

  for i := 1 to totalTufts do
  begin
    tries := 0;
    repeat
      tx := 6 + Random * (W - 12);
      ty := 14 + Random * (H - 20);
      Inc(tries);
      tooClose := False;
      for kCheck := 0 to nTufts - 1 do
      begin
        if Sqr(tx - tuftPts[kCheck].X) + Sqr(ty - tuftPts[kCheck].Y) < 144 then
        begin
          tooClose := True;
          Break;
        end;
      end;
    until (not tooClose and ((mM[Trunc(ty) * W + Trunc(tx)] > 0.4) or (Random < 0.25))) or (tries > 20);

    if not tooClose then
    begin
      DrawTuft(bmp, tx, ty);
      tuftPts[nTufts] := Point(Trunc(tx), Trunc(ty));
      Inc(nTufts);
    end;
  end;

  for i := 1 to Max(3, W * H div 60000) do
    DrawClump(bmp, 60 + Random * (W - 120), 80 + Random * (H - 160));

  SaveToJPEG(bmp, ExtractFilePath(ParamStr(0)) + 'BG.jpg', 85);

  Image321.Invalidate;
end;

procedure BikinTanahLumut(Image321: TImage32); overload;
begin
  BikinTanahLumut(Image321, Image321.Width, Image321.Height);
end;

procedure BikinTanahLumutMouseDown(Image321: TImage32; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  TerraHost(Image321).Image321MouseDown(nil, Button, Shift, X, Y, Layer);
end;

procedure BikinTanahLumutMouseMove(Image321: TImage32; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  TerraHost(Image321).Image321MouseMove(nil, Shift, X, Y, Layer);
end;

procedure BikinTanahLumutMouseUp(Image321: TImage32; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
  TerraHost(Image321).Image321MouseUp(nil, Button, Shift, X, Y, Layer);
end;

procedure BikinTanahLumutSimpan(Image321: TImage32);
begin
  TerraHost(Image321).ButtonSaveClick(nil);
end;

end.
