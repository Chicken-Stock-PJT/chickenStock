package com.example.chickenstock.ui.screens.search

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ArrowBack
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavController
import com.example.chickenstock.ui.theme.*
import com.example.chickenstock.navigation.Screen

data class SearchResult(
    val name: String,
    val code: String,
    val currentPrice: String,
    val priceChange: String,
    val changeRate: String,
    val isPositive: Boolean
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SearchScreen(navController: NavController) {
    var searchQuery by remember { mutableStateOf("") }
    
    // 임시 검색 결과 데이터
    val searchResults = remember {
        listOf(
            SearchResult("삼성전자", "005930", "77,800", "▲1,200", "+1.57%", true),
            SearchResult("SK하이닉스", "000660", "156,500", "▼2,300", "-1.45%", false),
            SearchResult("LG에너지솔루션", "373220", "412,000", "▲5,500", "+1.35%", true)
        )
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = {
                    TextField(
                        value = searchQuery,
                        onValueChange = { searchQuery = it },
                        placeholder = { Text("종목명, 코드를 검색하세요", fontFamily = SCDreamFontFamily) },
                        singleLine = true,
                        colors = TextFieldDefaults.colors(
                            focusedContainerColor = Gray50,
                            unfocusedContainerColor = Gray50,
                            disabledContainerColor = Gray50,
                            focusedIndicatorColor = Secondary500,
                            unfocusedIndicatorColor = Gray300,
                        ),
                        leadingIcon = {
                            Icon(
                                imageVector = Icons.Default.Search,
                                contentDescription = "검색",
                                tint = Gray700
                            )
                        },
                        modifier = Modifier.fillMaxWidth()
                    )
                },
                navigationIcon = {
                    IconButton(onClick = { navController.navigateUp() }) {
                        Icon(
                            imageVector = Icons.Default.ArrowBack,
                            contentDescription = "뒤로가기",
                            tint = Gray700
                        )
                    }
                },
                colors = TopAppBarDefaults.topAppBarColors(
                    containerColor = Gray0
                )
            )
        }
    ) { innerPadding ->
        LazyColumn(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding)
        ) {
            items(searchResults) { result ->
                SearchResultItem(result = result, onItemClick = {
                    navController.navigate(Screen.StockDetail.createRoute(result.code))
                })
                Divider(color = Gray100)
            }
        }
    }
}

@Composable
fun SearchResultItem(
    result: SearchResult,
    onItemClick: () -> Unit
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .clickable(onClick = onItemClick)
            .background(Gray0)
            .padding(horizontal = 16.dp, vertical = 12.dp),
        horizontalArrangement = Arrangement.SpaceBetween,
        verticalAlignment = Alignment.CenterVertically
    ) {
        Column {
            Text(
                text = result.name,
                fontSize = 16.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily
            )
            Spacer(modifier = Modifier.height(4.dp))
            Text(
                text = result.code,
                fontSize = 14.sp,
                color = Gray500,
                fontFamily = SCDreamFontFamily
            )
        }
        Column(horizontalAlignment = Alignment.End) {
            Text(
                text = result.currentPrice,
                fontSize = 16.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily
            )
            Spacer(modifier = Modifier.height(4.dp))
            Row(
                horizontalArrangement = Arrangement.spacedBy(4.dp),
                verticalAlignment = Alignment.CenterVertically
            ) {
                Text(
                    text = result.priceChange,
                    fontSize = 14.sp,
                    color = if (result.isPositive) Primary500 else Secondary500,
                    fontFamily = SCDreamFontFamily
                )
                Text(
                    text = result.changeRate,
                    fontSize = 14.sp,
                    color = if (result.isPositive) Primary500 else Secondary500,
                    fontFamily = SCDreamFontFamily
                )
            }
        }
    }
} 