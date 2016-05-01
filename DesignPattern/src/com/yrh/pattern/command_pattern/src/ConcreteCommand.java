package com.yrh.pattern.command_pattern.src;

/**
 * 具体命令类
 * @author Yrh
 *
 */
public class ConcreteCommand extends Command {

	public ConcreteCommand(Receiver receiver) {
		super(receiver);
	}

	@Override
	public void execute() {
		receiver.action();
	}
}
