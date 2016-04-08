package com.yrh.pattern.proxy_pattern.src;

/**
 * 真实实体类
 * @author Yrh
 *
 */
public class RealSubject implements Subject{
	
	@Override
	public void request() {
		System.out.println("真实的请求");
	}
}
