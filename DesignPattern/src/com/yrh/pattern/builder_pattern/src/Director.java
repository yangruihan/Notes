package com.yrh.pattern.builder_pattern.src;

public class Director {

	public void construct(Builder builder) {
		builder.buildPartA();
		builder.buildPartB();
	}
}
