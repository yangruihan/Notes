package com.yrh.pattern.adapter_pattern.src;

/**
 * 适配器
 * @author Yrh
 *
 */
public class Adapter implements Target {
	
	private Adaptee adaptee = new Adaptee();

	/**
	 * 将特殊请求转换成普通请求，以供客户端调用
	 */
	@Override
	public void request() {
		adaptee.specificRequest();
	}

}
