package com.yrh.pattern.command_pattern.src;

/**
 * 要求命令执行类
 * @author Yrh
 *
 */
public class Invoker {
	private Command command;

	public void setCommand(Command command) {
		this.command = command;
	}
	
	public void executeCommand() {
		command.execute();
	}
}
