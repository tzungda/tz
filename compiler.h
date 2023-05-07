
#ifndef tz_compiler_h
#define tz_compiler_h

#include "object.h"
#include "vm.h"

ObjFunction* compile( const char* source );
//p.508 To keep the compiler module cleanly separated from the rest of the VM, we'll do that in a separate function
void markCompilerRoots();

#endif
