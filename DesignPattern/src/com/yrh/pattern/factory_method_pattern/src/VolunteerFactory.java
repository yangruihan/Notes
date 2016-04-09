package com.yrh.pattern.factory_method_pattern.src;

/**
 * 社区志愿者工厂
 * @author Yrh
 *
 */
public class VolunteerFactory implements Factory{

	@Override
	public Leifeng createLeiFeng() {
		return new Volunteer();
	}
}
