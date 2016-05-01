package com.yrh.pattern.command_pattern.src;

/**
 * 抽象命令类
 * @author Yrh
 *
 */
public abstract class Command {
	protected Receiver receiver;
	
	public Command(Receiver receiver) {
		this.receiver = receiver;
	}
	
	public abstract void execute();
}
