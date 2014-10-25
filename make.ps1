$files = Get-ChildItem ".\src\*.erl"
foreach($f in $files) {
    erlc -W -o ebin $f.FullName
}
