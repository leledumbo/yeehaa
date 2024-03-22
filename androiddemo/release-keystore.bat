set JAVA_HOME=/usr/lib/jvm/java-8-openjdk
set PATH=%JAVA_HOME%/bin;%PATH%
set JAVA_TOOL_OPTIONS=-Duser.language=en
cd /mnt/Data/Works/Free/yeehaa/androiddemo
LC_ALL=C keytool -genkey -v -keystore androiddemo-release.keystore -alias androiddemo.keyalias -keyalg RSA -keysize 2048 -validity 10000 < /mnt/Data/Works/Free/yeehaa/androiddemo/keytool_input.txt
:Error
echo off
cls
echo.
echo Signature file created previously, remember that if you delete this file and it was uploaded to Google Play, you will not be able to upload another app without this signature.
echo.
pause
