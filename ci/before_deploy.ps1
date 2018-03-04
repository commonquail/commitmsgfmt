# This script takes care of packaging the build artifacts that will go in the
# release zipfile

$SRC_DIR = $PWD.Path
$STAGE = [System.Guid]::NewGuid().ToString()

Set-Location $Env:Temp
New-Item -Type Directory -Name $STAGE
Set-Location $STAGE

$PACKAGE = "${Env:CRATE_NAME}-${Env:APPVEYOR_REPO_TAG_NAME}-${Env:TARGET}"
New-Item -Type Directory -Name "$PACKAGE"

Set-Location "$PACKAGE"

Copy-Item "$SRC_DIR\LICENSE.txt" '.\'
Copy-Item "$SRC_DIR\README.md" '.\'
Copy-Item "$SRC_DIR\contrib"  '.\' -Recurse
Copy-Item "$SRC_DIR\target\${Env:TARGET}\release\${Env:CRATE_NAME}.exe" '.\'

Set-Location ..

$ZIP = "$SRC_DIR\${PACKAGE}.zip"

7z a "$ZIP" "$PACKAGE"

Push-AppveyorArtifact "$ZIP"

Set-Location ..
Remove-Item $STAGE -Force -Recurse
Set-Location $SRC_DIR
