#!/usr/bin/env bash

cd ../../


./gradlew assemble

clientId="..."      # Cookie 中的 __client_id
uid="..."           # Cookie 中的 _uid
beginX=0            # 开始绘画的 x 坐标
beginY=0            # 开始绘画的 y 坐标
imagePath="..."     # 图片绝对路径(只能包含有洛谷提供的颜色)
classpath="../luogu/build/libs/luogu-0.0.1.jar:../luogu/build/libs/luogu-0.0.1-dependencies.jar"

cd build/

cp "../scripts/src/paintBoard.kt" "internal-main.kts"

echo "

while (true) {
    try {
        LuoGu(\"${clientId}\", \"${uid}\").drawFromImage(${beginX}, ${beginY}, \"${imagePath}\".run(::File))
    } catch (e : Throwable) {
        e.printStackTrace()
    }
}
" >> internal-main.kts

kotlinc -jvm-target 1.8 -cp ${classpath} -script internal-main.kts
