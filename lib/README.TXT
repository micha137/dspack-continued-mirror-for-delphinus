
Borland C++Builder DirectX LIB files


This package includes "lib" files needed for Borland C++ Builder to compile 
DirectX 9 SDK programs.

To compile DirectX projects including Microsoft demos from DirectX 8 SDK you should make some simple things:
1) Add these libraries to include path.
2) Add some defines under "Project\Options\Directories/Conditionals\Conditional defines" like:
   "cosf=(float)cos;sinf=(float)sin;acosf=(float)acos;asinf=(float)asin;tanf=(float)tan;atanf=(float)atan;sqrtf=(float)sqrt"

3) To use D3DX9 library you should download special DLL (d3dx9ab.dll) and extract it in any common folder, like: "Windows\System".
4) To use DxErr9 library you should download special DLL (dxerr9ab.dll) and extract it in any common folder, like: "Windows\System".

You can download latest version of these files from: clootie.narod.ru/

Mail any questions to Alexey Barkovoy: clootie@reactor.ru

30-Dec-2002