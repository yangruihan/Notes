package com.yrh.pattern.singleton_pattern.src;

/**
 * 多线程单例模式
 * @author Yrh
 *
 */
public class SingletonMultiProcess {

	private static SingletonMultiProcess instance;
	private static byte[] lock = new byte[1];
	
	private SingletonMultiProcess() {
	}
	
	public static SingletonMultiProcess getInstance() {
		if (instance == null) {
			synchronized (lock) {
				if (instance == null) {
					instance = new SingletonMultiProcess();
				}
			}
		}
		
		return instance;
	}
}
