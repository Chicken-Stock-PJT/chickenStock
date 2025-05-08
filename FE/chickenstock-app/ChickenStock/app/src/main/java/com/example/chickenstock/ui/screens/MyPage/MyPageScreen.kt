package com.example.chickenstock.ui.screens.MyPage

import androidx.compose.foundation.layout.*
import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import com.example.chickenstock.ui.theme.Gray0
import com.example.chickenstock.ui.theme.Gray700
import com.example.chickenstock.ui.theme.SCDreamFontFamily

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun MyPageScreen(modifier: Modifier = Modifier) {
    Scaffold(
        containerColor = Gray0,
        topBar = {
            TopAppBar(
                title = { Text("마이페이지", color = Gray700, fontFamily = SCDreamFontFamily) },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = Gray0
                )
            )
        }
    ) { paddingValues ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(paddingValues)
        ) {
            Text(text = "Welcome to My Page!", modifier = modifier)
        }
    }
} 