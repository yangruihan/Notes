package com.yrh.pattern.strategy_pattern.src;

/**
 * 收费上下文类
 * @author Yrh
 *
 */
public class CashContext {

	private CashSuper cashSuper;
	
	public CashContext(String type, double money) {
		switch (type) {
		case "正常收费":
			cashSuper = new CashNormal(money);
			break;
			
		case "满300反100":
			cashSuper = new CashReturn(money, 300, 100);
			break;
			
		case "打8折":
			cashSuper = new CashRebate(money, 0.8);
			break;
			
		default:
			break;
		}
	}
	
	public double getResult() {
		return cashSuper.acceptCash();
	}
}
