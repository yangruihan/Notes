package com.yrh.pattern.builder_pattern.src;

/**
 * 抽象建造者类
 * @author Yrh
 *
 */
public abstract class Builder {
	public abstract void buildPartA();
	public abstract void buildPartB();
	public abstract Product getResult();
}
