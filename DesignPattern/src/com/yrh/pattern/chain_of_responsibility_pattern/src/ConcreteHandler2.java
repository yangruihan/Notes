package com.yrh.pattern.chain_of_responsibility_pattern.src;

/**
 * 具体处理类2
 * @author Yrh
 *
 */
public class ConcreteHandler2 extends Handler {

	@Override
	public void handleRequest(int request) {
		if (request >= 10 && request < 20) {
			System.out.println(this.getClass().getName() + " 处理请求 " + request);
		} else if (successor != null) {
			successor.handleRequest(request); // 如果继任者不为空，则交由继任者处理
		}
	}
}
