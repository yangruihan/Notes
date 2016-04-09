package com.yrh.pattern.proxy_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Proxy proxy = new Proxy();
		
		proxy.request();
	}
}
