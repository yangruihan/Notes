package com.yrh.pattern.facade_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Facade f = new Facade();
		
		f.methodA();
		System.out.println("-----------");
		
		f.methodB();
		System.out.println("-----------");
		
		f.methodAll();
	}
}
