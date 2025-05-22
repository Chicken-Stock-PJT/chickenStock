package com.example.chickenstock.ui.screens.login

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavController
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.viewmodel.MainViewModel

@Composable
fun SignupSuccessScreen(
    navController: NavController,
    viewModel: MainViewModel
) {
    // 하단바와 탑바 숨기기
    LaunchedEffect(Unit) {
        viewModel.setBottomBarVisibility(false)
        viewModel.setTopBarVisibility(false)
    }

    Column(
        modifier = Modifier
            .fillMaxSize()
            .background(Color(0xFFF5F5F5))
            .padding(24.dp),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center
    ) {
        Text(
            text = "회원가입에 성공했습니다!",
            fontSize = 24.sp,
            fontWeight = FontWeight.Bold,
            fontFamily = SCDreamFontFamily,
            modifier = Modifier.padding(bottom = 32.dp)
        )

        Button(
            onClick = {
                navController.navigate(Screen.Home.route) {
                    popUpTo(0) { inclusive = true }
                }
            },
            colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
            modifier = Modifier
                .fillMaxWidth()
                .height(50.dp),
            shape = RoundedCornerShape(12.dp)
        ) {
            Text(
                "시작하기",
                color = Color.Black,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily,
                fontSize = 18.sp
            )
        }
    }
} 