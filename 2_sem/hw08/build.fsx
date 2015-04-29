#!packages/FAKE/tools/FAKE.exe
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake

let buildDir = "./build/"
let testDir  = "./test/"

RestorePackages()

Target "Clean" (fun _ ->
  CleanDirs [buildDir; testDir]
)

Target "BuildApp" (fun _ ->
  !! "src/app/**/*.fsproj"
    |> MSBuildRelease buildDir "Build"
    |> Log "AppBuild-Output: "
)





"Clean"
  ==> "BuildApp"


 

RunTargetOrDefault "BuildApp"
