package com.example.chickenstock.ui.screens.mypage

import androidx.compose.foundation.layout.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import com.example.chickenstock.ui.theme.Gray0
import com.example.chickenstock.ui.theme.Gray700
import com.example.chickenstock.ui.theme.SCDreamFontFamily

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun MyPageScreen(navController: NavController) {
    Scaffold(
        containerColor = Gray0,
        topBar = {
            TopAppBar(
                title = {
                    Text(
                        text = "마이페이지",
                        fontFamily = SCDreamFontFamily
                    )
                },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = Gray0
                )
            )
        }
    ) { innerPadding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding)
        ) {
            // 마이페이지 화면 내용
            Text("마이페이지 화면")
        }
    }
} 