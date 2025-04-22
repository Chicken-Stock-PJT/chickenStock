package com.example.chickenstock.ui.screens

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.Icon
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.example.chickenstock.R
import androidx.compose.foundation.layout.size
import androidx.compose.material3.BorderStroke
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.width

@Composable
fun HomeScreen(modifier: Modifier = Modifier) {
    Column(modifier = modifier.fillMaxWidth().background(Color(0xFFFFFBEB)).padding(24.dp)) {
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically
        ) {
            Column {
                Text(text = "투자의 시작", fontSize = 36.sp, fontWeight = FontWeight.Bold)
                Text(text = "치킨스톡과 함께", fontSize = 36.sp, fontWeight = FontWeight.Bold)
            }
            Icon(
                painter = painterResource(id = R.drawable.logo),
                contentDescription = "Logo",
                tint = Color.Unspecified,
                modifier = Modifier.size(80.dp)
            )
        }
        Row(
            modifier = Modifier.fillMaxWidth().padding(top = 16.dp),
            horizontalArrangement = Arrangement.Start
        ) {
            Button(onClick = { /* 회원가입 기능 */ }, colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)), modifier = Modifier.width(100.dp)) {
                Text("회원가입", color = Color.Black)
            }
            Button(onClick = { /* 로그인 기능 */ }, colors = ButtonDefaults.buttonColors(containerColor = Color.White), modifier = Modifier.width(100.dp).padding(start = 8.dp), border = BorderStroke(1.dp, Color.Gray)) {
                Text("로그인", color = Color.Black)
            }
        }
    }
} 