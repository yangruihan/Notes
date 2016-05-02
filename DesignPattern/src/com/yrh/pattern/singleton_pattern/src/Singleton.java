package com.yrh.pattern.singleton_pattern.src;

/**
 * 单例类
 * @author Yrh
 *
 */
public class Singleton {
	
	private static Singleton instance;
	
	/**
	 * 私有的构造方法
	 */
	private Singleton() {
	}

	public static Singleton getInstance() {
		if (instance == null) {
			instance = new Singleton();
		}
		
		return instance;
	}
}
