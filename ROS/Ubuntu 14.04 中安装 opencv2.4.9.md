# Ubuntu 14.04 中安装 opencv2.4.9
## 下载及安装
- 从 http://jaist.dl.sourceforge.net/project/opencvlibrary/opencv-unix/2.4.9/opencv-2.4.9.zip 获取OpenCV源码

- 将源码解压到任意目录 
```
$ unzip opencv-2.4.9.zip
```

- 进入源码目录，创建 release 目录
```
$ cd opencv-2.4.9
$ mkdir release
```

- 编译源码之前需要安装一些软件
```
$ sudo apt-get install build-essential cmake libgtk2.0-dev pkg-config python-dev python-numpy libavcodec-dev libavformat-dev libswscale-dev
```

- 进入 release 目录，并编译源码
```
$ cd release
$ cmake -D CMAKE_BUILD_TYPE=RELEASE -D CMAKE_INSTALL_PREFIX=/usr/local ..
```

- 安装
```
$ sudo make install
```

## 测试
- 任意创建一个目录，建立一个 test.cpp 文件，代码如下
```[cpp]
#include <cv.h>  
#include <highgui.h>  
  
using namespace cv;  
  
int main(int argc, char* argv[])  
{  
    Mat image;  
    image = imread(argv[1], 1);  
  
    if (argc != 2 || !image.data)   
    {  
        printf("No image data\n");  
        return -1;  
    }  
  
    namedWindow("Display Image", CV_WINDOW_AUTOSIZE);  
    imshow("Display Image", image);  
    waitKey(0);  
    return 0;  
}
```
- 编写 CMakeLists.txt 文件，内容如下：
```
project(test)  
find_package(OpenCV REQUIRED)  
add_executable(test test)  
target_link_libraries(test ${OpenCV_LIBS})  
cmake_minimum_required(VERSION 2.8)
```

- 编译+运行，在该目录下输入如下命令：
```
$ cmake .
$ make
$ ./test <图片名>
```
成功显示图片则说明 OpenCV 安装成功
