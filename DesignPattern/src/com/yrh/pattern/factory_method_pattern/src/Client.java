package com.yrh.pattern.factory_method_pattern.src;

/**
 * 客户端
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Factory factory = new UndergraduateFactory();
		Leifeng leifeng = factory.createLeiFeng();
		
		leifeng.buyRice();
		leifeng.sweep();
		leifeng.wash();
	}
}
