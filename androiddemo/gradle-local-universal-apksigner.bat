set Path=%PATH%;/opt/LAMW/sdk/platform-tools;/opt/LAMW/sdk/build-tools\29.0.3
set GRADLE_HOME=/opt/LAMW/gradle/
set PATH=%PATH%;%GRADLE_HOME%\bin
zipalign -v -p 4 /mnt/Data/Works/Free/yeehaa/androiddemo\build\outputs\apk\release\androiddemo-universal-release-unsigned.apk /mnt/Data/Works/Free/yeehaa/androiddemo\build\outputs\apk\release\androiddemo-universal-release-unsigned-aligned.apk
apksigner sign --ks /mnt/Data/Works/Free/yeehaa/androiddemo\androiddemo-release.keystore --ks-pass pass:123456 --key-pass pass:123456 --out /mnt/Data/Works/Free/yeehaa/androiddemo\build\outputs\apk\release\androiddemo-release.apk /mnt/Data/Works/Free/yeehaa/androiddemo\build\outputs\apk\release\androiddemo-universal-release-unsigned-aligned.apk
