#if INTERACTIVE
#load "../FSharp.Data.Gherkin.DesignTime/ProvidedTypes.fsi" "../FSharp.Data.Gherkin.DesignTime/ProvidedTypes.fs"
#endif

namespace GherkinProvider.Runtime

type AssemblyChecker() = 
    static member Check() = true


#if !IS_DESIGNTIME
// Put the TypeProviderAssemblyAttribute in the runtime DLL, pointing to the design-time DLL
[<assembly:CompilerServices.TypeProviderAssembly("FSharp.Data.Gherkin.DesignTime.dll")>]
do ()
#endif