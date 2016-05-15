# Java Socket 编程入门
## 一、网络编程中的两个主要问题
- 如何准确的定位网络上一台或多台主机

- 找到主机后如何可靠高效的进行数据传输

在 TCP/IP 协议中 IP 层主要负责网络主机的定位，数据传输的路由，由 IP 地址可以唯一地确定 Internet 上的一台主机。

而 TCP 层则提供面向应用的可靠（TCP）的或非可靠（UDP）的数据传输机制，这是网络编程的主要对象，一般不需要关心 IP 层是如何处理数据的。

目前较为流行的网络编程模型是客户机/服务器（C/S）结构。即通信双方一方作为服务器等待客户提出请求并予以响应。客户则在需要服务时向服务器提出申请。服务器一般作为守护进程始终运行，监听网络端口，一旦有客户请求，就会启动一个服务进程来响应该客户，同时自己继续监听服务端口，使后来的客户也能及时得到服务。

## 二、两类传输协议：TCP、UDP
### TCP
TCP 是 Tranfer Control Protocol 的简称，是一种面向连接的保证**可靠传输的协议**。通过 TCP 协议传输，得到的是一个**顺序的无差错的数据流**。发送方和接收方的成对的两个 socket 之间必须建 立连接，以便在 TCP 协议的基础上进行通信，当一个 socket（通常都是server socket）等待建立连接时，另一个 socket 可以要求进行连接，一旦这两个 socket 连接起来，它们就可以进行双向数据传输，双方都可以进行发送或接收操作。

### UDP
UDP 是 User Datagram Protocol 的简称，是一种**无连接的协议**，每个数据报都是一个独立的信息，包括完整的源地址或目的地址，**它在网络上以任何可能的路径传往目的地**，因此能否到达目的地，到达目的地的时间以及内容的正确性都是不能被保证的。

### TCP与UDP的比较
- UDP：

    1. 每个数据报中都给出了完整的地址信息，因此无需要建立发送方和接收方的连接

    2. **UDP 传输数据**时是有**大小限制**的，每个被传输的数据报**必须**限定在**64KB**之内。

    3. UDP是一个**不可靠的协议**，发送方所发送的数据报并不一定以相同的次序到达接收方

- TCP：

    1. **面向连接的协议**，在 socket 之间进行数据传输之前必然要建立连接，所以在 TCP 中需要连接时间

    2. TCP 传输数据**无大小限制**，一旦连接建立起来，双方的 socket 就可以按统一的格式传输大的数据

    3. TCP 是一个**可靠的协议**，它确保接收方完全正确地获取发送方所发送的全部数据

### TCP与UDP的应用
1. **TCP** 在网络通信上有极强的生命力，例如远程连接（Telnet）和文件传输（FTP）都需要不定长度的数据被可靠地传输。但是可靠的传输是要付出代价的，对数据内容正确性的检验必然占用计算机的处理时间和网络的带宽，因此 **TCP 传输的效率不如 UDP 高**

2. **UDP** 操作简单，而且仅需要较少的监护，因此通常用于局域网高可靠性的分散系统中 client/server 应用程序。例如视频会议系统，并不要求音频视频数据绝对的正确，只要保证连贯性就可以了，这种情况下显然使用 UDP 会更合理一些

## 三、基于 Socket 的 Java 网络编程
### 什么是 Socket
网络上的两个程序通过一个双向的通讯连接实现数据的交换，这个双向链路的一端称为一个 **Socket**。Socket 通常用来实现客户方和服务方的连接。Socket 是 TCP/IP 协议的一个十分流行的编程界面，一个 Socket 由一个 IP 地址和一个端口号唯一确定。

但是，Socket 所支持的协议种类也不光 TCP/IP 一种，因此两者之间是没有必然联系的。在 Java 环境下，Socket 编程主要是指基于 TCP/IP 协议的网络编程。

### Socket 通讯的过程
Server 端 Listen（监听）某个端口是否有连接请求，Client 端向 Server 端发出 Connect（连接）请求，Server 端向 Client 端发回 Accept（接受）消息。一个连接就建立起来了。Server 端和 Client 端都可以通过`Send`、`Write`等方法与对方通信。

对于一个功能齐全的Socket，都要包含以下基本结构，其工作过程包含以下四个基本的步骤：

1. 创建 Socket

2. 打开连接到 Socket 的输入/出流

3. 按照一定的协议对 Socket 进行读/写操作

4. 关闭Socket

### 创建 Socket
Java 在包`java.net`中提供了两个类`Socket`和`ServerSocket`，分别用来表示双向连接的客户端和服务端。这是两个封装得非常好的类，使用很方便。其构造方法如下：

```java
Socket(InetAddress address, int port);

Socket(InetAddress address, int port, boolean stream);

Socket(String host, int port);

Socket(String host, int port, boolean stream);

Socket(SocketImpl impl)

Socket(String host, int port, InetAddress localAddr, int localPort)

Socket(InetAddress address, int port, InetAddress localAddr, int localPort)

ServerSocket(int port);

ServerSocket(int port, int backlog);

ServerSocket(int port, int backlog, InetAddress bindAddr)
```

其中`address`、`host`和`port`分别是双向连接中另一方的 IP 地址、主机名和端口号，`stream`指明`Socket`是**流 socket** 还是**数据报 socket**，`localPort`表示本地主机的端口号，`localAddr`和`bindAddr`是本地机器的地址（`ServerSocket`的主机地址），`impl`是`Socket`的父类，既可以用来创建`ServerSocket`又可以用来创建`Socket`。`count`则表示服务端所能支持的最大连接数。例如：

```java
Socket client = new Socket("127.0.01.", 80);

ServerSocket server = new ServerSocket(80);
```

注意，在选择端口时，必须小心。每一个端口提供一种特定的服务，只有给出正确的端口，才能获得相应的服务。0~1023 的端口号为系统所保留，例如 http 服务的端口号为 80，telnet 服务的端口号为 21，ftp 服务的端口号为 23，所以我们在选择端口号时，最好选择一个大于 1023 的数以防止发生冲突。

在创建 socket 时如果发生错误，将产生`IOException`，在程序中必须对之作出处理。所以在创建`Socket`或`ServerSocket`是必须捕获或抛出例外。

### 简单的 Client/Server 程序
#### 客户端代码
```java
package Socket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

/**
 * Created by yangruihan on 16/5/14.
 * 客户端类
 */
public class Client {

    public static void main(String[] args) {
        try {
            // 向本机的4700端口发出客户请求
            Socket socket = new Socket("127.0.0.1", 4700);

            // 由系统标准输入设备构造 BufferedReader 对象
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));

            // 由 Socket 对象得到输出流,并构造 PrintWriter 对象
            PrintWriter os = new PrintWriter(socket.getOutputStream());

            // 由 Socket 对象得到输入流,并构造相应的 BufferedReader 对象
            BufferedReader is = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            String readLine;

            // 从系统标准输入读入一字符串
            readLine = bufferedReader.readLine();

            while (!readLine.equals("exit")) {
                // 将从系统标准输入的字符串输出到 Server
                os.println(readLine);
                // 刷新输出流,使 Server 马上收到该字符串
                os.flush();

                // 在系统标准输出上打印读入的字符串
                System.out.println("Client:" + readLine);

                // 从 Server 读入一字符串,并打印到标准输出上
                System.out.println("Server:" + is.readLine());

                readLine = bufferedReader.readLine();
            }

            // 关闭 Socket 输出流
            os.close();

            // 关闭 Socket 输入流
            is.close();

            // 关闭 Socket
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}
```

#### 服务器代码
```java
package Socket;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Created by yangruihan on 16/5/14.
 * 服务器类
 */
public class Server {

    public static void main(String[] args) {

        ServerSocket serverSocket = null;

        try {
            // 创建一个 ServerSocket 在端口 4700 监听客户请求
            serverSocket = new ServerSocket(4700);
        } catch (IOException e) {
            e.printStackTrace();
        }

        Socket socket = null;

        try {
            // 使用 accept() 阻塞等待客户请求,有客户请求到则产生一个 Socket 对象,并继续执行
            socket = serverSocket.accept();
        } catch (IOException e) {
            e.printStackTrace();
        }

        String readLine;

        try {
            // 由 Socket 对象得到输入流,并构造相应的 BufferedReader 对象
            BufferedReader is = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            // 由 Socket 对象得到输出流,并构造 PrintWriter 对象
            PrintWriter os = new PrintWriter(socket.getOutputStream());

            // 由系统标准输入设备构造 BufferedReader 对象
            BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));

            // 在标准输出上打印从客户端读入的字符串
            System.out.println("Client:" + is.readLine());

            // 从系统标准输入读入一字符串
            readLine = bufferedReader.readLine();

            while (!readLine.equals("exit")) {
                // 将从系统标准输入的字符串输出到 Server
                os.println(readLine);
                // 刷新输出流,使 Server 马上收到该字符串
                os.flush();

                // 在系统标准输出上打印读入的字符串
                System.out.println("Server:" + readLine);

                // 从 Server 读入一字符串,并打印到标准输出上
                System.out.println("Client:" + is.readLine());

                readLine = bufferedReader.readLine();
            }

            // 关闭 Socket 输出流
            os.close();

            // 关闭 Socket 输入流
            is.close();

            // 关闭 Socket
            socket.close();

            // 关闭 SocketSocket
            serverSocket.close();

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```