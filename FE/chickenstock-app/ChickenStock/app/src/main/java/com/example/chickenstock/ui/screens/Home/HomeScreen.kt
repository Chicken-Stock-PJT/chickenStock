package com.example.chickenstock.ui.screens.home

import androidx.compose.foundation.background
import androidx.compose.foundation.border
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowRight
import androidx.compose.material.icons.outlined.FavoriteBorder
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavHostController
import androidx.compose.ui.graphics.Shape
import com.example.chickenstock.R
import com.example.chickenstock.navigation.Screen
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.clickable
import coil.compose.AsyncImage
import com.example.chickenstock.viewmodel.MainViewModel
import com.example.chickenstock.ui.components.SegmentedControl
import com.example.chickenstock.ui.components.StockListItem
import com.example.chickenstock.ui.components.MoreButton
import com.example.chickenstock.ui.components.StockItem
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun HomeScreen(
    navController: NavHostController,
    viewModel: MainViewModel
) {
    val sortOptions = listOf("거래대금", "등락률", "거래량")
    var selectedSortIndex by remember { mutableStateOf(0) }

    Scaffold(
        containerColor = Color.White
    ) { innerPadding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding)
        ) {
            LazyColumn(modifier = Modifier.fillMaxWidth()) {
                // 배너 영역
                items(1) {
                    Column(
                        modifier = Modifier
                            .fillMaxWidth()
                            .background(Color(0xFFFFFBEB))
                            .padding(vertical = 42.dp, horizontal = 24.dp)
                    ) {
                        Row(
                            modifier = Modifier.fillMaxWidth(),
                            verticalAlignment = Alignment.CenterVertically
                        ) {
                            Column(modifier = Modifier.weight(1f)) {
                                Text("투자의 시작", fontSize = 32.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily)
                                Spacer(modifier = Modifier.height(12.dp))
                                Text("치킨스톡과 함께", fontSize = 32.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily)
                            }
                            Icon(
                                painter = painterResource(id = R.drawable.logo),
                                contentDescription = "Logo",
                                tint = Color.Unspecified,
                                modifier = Modifier.size(120.dp)
                            )
                        }
                        Row(
                            modifier = Modifier.fillMaxWidth().padding(top = 16.dp)
                        ) {
                            Button(
                                onClick = { },
                                colors = ButtonDefaults.buttonColors(containerColor = Color(0xFFFFEB3B)),
                                modifier = Modifier.width(90.dp).height(40.dp),
                                shape = RoundedCornerShape(12.dp),
                                contentPadding = PaddingValues(vertical = 10.dp)
                            ) {
                                Text("회원가입", color = Color.Black, fontWeight = FontWeight.W500, fontFamily = SCDreamFontFamily, fontSize = 16.sp)
                            }
                            Button(
                                onClick = { },
                                colors = ButtonDefaults.buttonColors(containerColor = Color.White),
                                modifier = Modifier.width(90.dp).height(40.dp).padding(start = 16.dp),
                                shape = RoundedCornerShape(12.dp),
                                border = BorderStroke(1.dp, Color(0xFFC7C7C7)),
                                contentPadding = PaddingValues(vertical = 10.dp)
                            ) {
                                Text("로그인", color = Color.Black, fontWeight = FontWeight.W500, fontFamily = SCDreamFontFamily, fontSize = 16.sp)
                            }
                        }
                    }
                }

                // 실시간 종목 랭킹 영역
                items(1) {
                    Column(
                        modifier = Modifier
                            .fillMaxWidth()
                            .background(Color.White)
                            .padding(vertical = 24.dp, horizontal = 32.dp)
                    ) {
                        Text("실시간 종목 랭킹", fontSize = 24.sp, fontWeight = FontWeight.W700, fontFamily = SCDreamFontFamily)
                        Spacer(modifier = Modifier.height(16.dp))

                        SegmentedControl(
                            items = sortOptions,
                            selectedIndex = selectedSortIndex,
                            onSelectedIndexChange = { selectedSortIndex = it }
                        )

                        Spacer(modifier = Modifier.height(12.dp))

                        val items = listOf(
                            StockItem("086520", "에코프로", "KOSDAQ", "1230000", "+5.60", "1200"),
                            StockItem("005930", "삼성전자", "KOSPI", "73200", "-0.40", "950"),
                            StockItem("035720", "카카오", "KOSPI", "48500", "-1.20", "850")
                        )

                        items.forEach { stock ->
                            StockListItem(
                                stock = stock,
                                navController = navController,
                                onFavoriteClick = { /* 즐겨찾기 기능 */ }
                            )
                        }

                        Spacer(modifier = Modifier.height(16.dp))

                        MoreButton(
                            onClick = {
                                navController.navigate(Screen.Stock.route) {
                                    launchSingleTop = true
                                    restoreState = true
                                    popUpTo(navController.graph.startDestinationId) {
                                        saveState = true
                                    }
                                }
                            }
                        )
                    }
                }
            }
        }
    }
}
