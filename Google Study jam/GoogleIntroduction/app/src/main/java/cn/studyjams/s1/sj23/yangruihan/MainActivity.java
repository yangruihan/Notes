package cn.studyjams.s1.sj23.yangruihan;

import android.content.Intent;
import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;

public class MainActivity extends AppCompatActivity implements View.OnClickListener {

    private Button showHistoryBtn;
    private Button showProductBtn;
    private Button showCultureBtn;
    private Button showDisputeBtn;
    private Button showFaultBtn;


    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        initBtn();
    }

    private void initBtn() {
        showHistoryBtn = (Button) findViewById(R.id.intro_history_btn);
        showProductBtn = (Button) findViewById(R.id.intro_product_btn);
        showCultureBtn = (Button) findViewById(R.id.intro_culture_btn);
        showDisputeBtn = (Button) findViewById(R.id.intro_dispute_btn);
        showFaultBtn = (Button) findViewById(R.id.intro_fault_btn);

        showHistoryBtn.setOnClickListener(this);
        showProductBtn.setOnClickListener(this);
        showCultureBtn.setOnClickListener(this);
        showDisputeBtn.setOnClickListener(this);
        showFaultBtn.setOnClickListener(this);
    }

    @Override
    public void onClick(View v) {
        Intent intent;
        switch (v.getId()) {
            case R.id.intro_history_btn:
                intent = new Intent(this, IntroHistoryActivity.class);
                startActivity(intent);
                break;

            case R.id.intro_product_btn:
                intent = new Intent(this, IntroProductActivity.class);
                startActivity(intent);
                break;

            case R.id.intro_culture_btn:
                intent = new Intent(this, IntroCultureActivity.class);
                startActivity(intent);
                break;

            case R.id.intro_dispute_btn:
                intent = new Intent(this, IntroDisputeActivity.class);
                startActivity(intent);
                break;

            case R.id.intro_fault_btn:
                intent = new Intent(this, IntroFaultActivity.class);
                startActivity(intent);
                break;
        }
    }
}
