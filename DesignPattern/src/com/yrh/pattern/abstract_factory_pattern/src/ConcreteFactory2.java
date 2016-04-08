package com.yrh.pattern.abstract_factory_pattern.src;

/**
 * 具体工厂2
 * @author Yrh
 *
 */
public class ConcreteFactory2 extends Factory {

	@Override
	public ProductA getProductA() {
		return new ProductA2();
	}

	@Override
	public ProductB getProductB() {
		return new ProductB2();
	}

}
