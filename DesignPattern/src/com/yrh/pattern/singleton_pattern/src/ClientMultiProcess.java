package com.yrh.pattern.singleton_pattern.src;

/**
 * 多线程测试客户端
 * 
 * @author Yrh
 *
 */
public class ClientMultiProcess {

	public static void main(String[] args) {
		Thread a = new Thread(new Runnable() {

			@Override
			public void run() {
				SingletonMultiProcess s = SingletonMultiProcess.getInstance();
				System.out.println(s.hashCode());
			}
		});

		Thread b = new Thread(new Runnable() {

			@Override
			public void run() {
				SingletonMultiProcess s = SingletonMultiProcess.getInstance();
				System.out.println(s.hashCode());
			}
		});
		
		Thread c = new Thread(new Runnable() {

			@Override
			public void run() {
				SingletonMultiProcess s = SingletonMultiProcess.getInstance();
				System.out.println(s.hashCode());
			}
		});
		
		a.start();
		b.start();
		c.start();
	}
}
