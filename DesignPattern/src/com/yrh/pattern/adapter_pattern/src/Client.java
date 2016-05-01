package com.yrh.pattern.adapter_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Target target = new Adapter();
		
		target.request();
	}
}
