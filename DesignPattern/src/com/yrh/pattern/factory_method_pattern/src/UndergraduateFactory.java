package com.yrh.pattern.factory_method_pattern.src;

/**
 * 学雷锋的大学生工厂
 * @author Yrh
 *
 */
public class UndergraduateFactory implements Factory {

	@Override
	public Leifeng createLeiFeng() {
		return new Undergraduate();
	}
}
