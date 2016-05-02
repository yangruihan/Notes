package com.yrh.pattern.mediator_pattern.src;

/**
 * 抽象同事类
 * @author Yrh
 *
 */
public abstract class Mediator {

	public abstract void send(String message, Colleague colleague);
}
