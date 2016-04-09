package com.yrh.pattern.proxy_pattern.src;

/**
 * 代理类
 * @author Yrh
 *
 */
public class Proxy implements Subject {

	private RealSubject realSubject; // 持有真实实体的引用
	
	@Override
	public void request() {
		if (realSubject == null) {
			realSubject = new RealSubject();
		}
		
		System.out.println("事务之前");
		
		// 执行事务
		realSubject.request();
		
		System.out.println("事务之后");
	}
}
