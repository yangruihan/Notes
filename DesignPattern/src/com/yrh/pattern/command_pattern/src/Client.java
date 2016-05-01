package com.yrh.pattern.command_pattern.src;

/**
 * 客户端类
 * @author Yrh
 *
 */
public class Client {

	public static void main(String[] args) {
		Receiver r = new Receiver();
		Command c = new ConcreteCommand(r);
		Invoker i = new Invoker();
		
		i.setCommand(c);
		i.executeCommand();
	}
}
