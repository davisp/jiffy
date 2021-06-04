$rootdir = split-path -parent $MyInvocation.MyCommand.Definition
Push-Location $rootdir
[Environment]::CurrentDirectory = $PWD

if ((Get-Command "rebar.cmd" -ErrorAction SilentlyContinue) -eq $null)
{
   if (-Not (Test-Path "src\rebar"))
   {
      git clone --depth 1 https://github.com/apache/couchdb-rebar.git $rootdir\src\rebar
   }
   cmd /c "cd src\rebar && $rootdir\src\rebar\bootstrap.bat"
   if (-Not (Test-Path "bin"))
   {
      mkdir $rootdir\bin
   }
   cp $rootdir\src\rebar\rebar $rootdir\bin\rebar
   cp $rootdir\src\rebar\rebar.cmd $rootdir\bin\rebar.cmd
   make -C $rootdir\src\rebar clean
   rmdir $rootdir\src\rebar
   $env:Path += ";$rootdir\bin"
}

if ((Get-Command "enc.cmd" -ErrorAction SilentlyContinue) -eq $null)
{
   if (-Not (Test-Path "bin"))
   {
      git clone --depth 1 https://github.com/davisp/erlang-native-compiler.git $rootdir\src\erlang-native-compiler
   }
   cmd /c "cd src\erlang-native-compiler && $rootdir\src\erlang-native-compiler\bootstrap.bat"
   if (-Not (Test-Path "bin"))
   {
      mkdir $rootdir\bin
   }
   cp $rootdir\src\erlang-native-compiler\enc $rootdir\bin\enc
   cp $rootdir\src\erlang-native-compiler\enc.cmd $rootdir\bin\enc.cmd
   make -C $rootdir\src\erlang-native-compiler clean
   rmdir $rootdir\src\erlang-native-compiler
   $env:Path += ";$rootdir\bin"
}
