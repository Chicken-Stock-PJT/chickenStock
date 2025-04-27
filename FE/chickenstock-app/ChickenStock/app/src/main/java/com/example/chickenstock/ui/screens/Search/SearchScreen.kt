package com.example.chickenstock.ui.screens.search

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.ArrowBack
import androidx.compose.material.icons.filled.ArrowDownward
import androidx.compose.material.icons.filled.ArrowUpward
import androidx.compose.material.icons.filled.Close
import androidx.compose.material.icons.filled.Search
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavController
import com.example.chickenstock.ui.theme.*
import com.example.chickenstock.navigation.Screen
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import androidx.activity.compose.BackHandler
import androidx.compose.ui.draw.clip

data class SearchResult(
    val name: String,
    val code: String,
    val currentPrice: String,
    val priceChange: String,
    val changeRate: String,
    val isPositive: Boolean
)

data class PopularSearchItem(
    val rank: Int,
    val keyword: String,
    val trend: String // "UP", "DOWN", "SAME"
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun SearchScreen(navController: NavController) {
    var searchQuery by remember { mutableStateOf("") }
    val keyboardController = LocalSoftwareKeyboardController.current
    
    // 뒤로가기 버튼 처리
    BackHandler {
        keyboardController?.hide()
        navController.navigateUp()
    }
    
    // 최근 검색어 데이터
    val recentSearches = remember { listOf("삼성") }
    
    // 인기 검색어 데이터
    val popularSearches = remember {
        listOf(
            PopularSearchItem(1, "삼성", "UP"),
            PopularSearchItem(2, "엔비디아", "SAME"),
            PopularSearchItem(3, "대한제당", "DOWN"),
            PopularSearchItem(4, "네이버", "DOWN"),
            PopularSearchItem(5, "테슬라", "DOWN"),
            PopularSearchItem(6, "닌텐도", "SAME")
        )
    }

    Scaffold(
        containerColor = Color.White
    ) { innerPadding ->
        Column(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding)
                .padding(horizontal = 20.dp)
        ) {
            Spacer(modifier = Modifier.height(16.dp))
            
            // 최근 검색 섹션
            Text(
                text = "최근 검색",
                fontSize = 18.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily,
                modifier = Modifier.padding(bottom = 12.dp)
            )
            
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp)
            ) {
                if (recentSearches.isEmpty()) {
                    Text(
                        text = "최근 검색어가 없습니다.",
                        color = Gray500,
                        fontFamily = SCDreamFontFamily,
                        fontSize = 14.sp
                    )
                } else {
                    recentSearches.forEach { keyword ->
                        RecentSearchChip(
                            keyword = keyword,
                            onRemove = { /* 검색어 삭제 구현 */ }
                        )
                    }
                }
            }
            
            Spacer(modifier = Modifier.height(32.dp))
            
            // 인기 검색 섹션
            Text(
                text = "인기 검색",
                fontSize = 18.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily,
                modifier = Modifier.padding(bottom = 12.dp)
            )
            
            Column(
                verticalArrangement = Arrangement.spacedBy(12.dp)
            ) {
                for (i in 0 until popularSearches.size step 2) {
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        horizontalArrangement = Arrangement.spacedBy(8.dp)
                    ) {
                        // 왼쪽 항목
                        PopularSearchItemView(
                            item = popularSearches[i],
                            modifier = Modifier.weight(1f),
                            onClick = { /* 검색어 클릭 처리 */ }
                        )
                        
                        // 오른쪽 항목 (있는 경우)
                        if (i + 1 < popularSearches.size) {
                            PopularSearchItemView(
                                item = popularSearches[i + 1],
                                modifier = Modifier.weight(1f),
                                onClick = { /* 검색어 클릭 처리 */ }
                            )
                        } else {
                            Spacer(modifier = Modifier.weight(1f))
                        }
                    }
                }
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun RecentSearchChip(
    keyword: String,
    onRemove: () -> Unit
) {
    Surface(
        shape = RoundedCornerShape(16.dp),
        color = Color(0xFFFFF8D6),
        modifier = Modifier.height(32.dp)
    ) {
        Row(
            modifier = Modifier.padding(horizontal = 12.dp),
            verticalAlignment = Alignment.CenterVertically
        ) {
            Text(
                text = keyword,
                color = Color.Black,
                fontSize = 14.sp,
                fontFamily = SCDreamFontFamily
            )
            
            Spacer(modifier = Modifier.width(4.dp))
            
            Icon(
                imageVector = Icons.Default.Close,
                contentDescription = "삭제",
                tint = Gray500,
                modifier = Modifier
                    .size(16.dp)
                    .clickable { onRemove() }
            )
        }
    }
}

@Composable
fun PopularSearchItemView(
    item: PopularSearchItem,
    modifier: Modifier = Modifier,
    onClick: () -> Unit
) {
    Row(
        modifier = modifier
            .fillMaxWidth()
            .clickable { onClick() },
        verticalAlignment = Alignment.CenterVertically
    ) {
        // 순위
        Text(
            text = item.rank.toString(),
            color = Color(0xFFFFC107),
            fontSize = 16.sp,
            fontWeight = FontWeight.Bold,
            fontFamily = SCDreamFontFamily,
            modifier = Modifier.padding(end = 12.dp)
        )
        
        // 검색어
        Text(
            text = item.keyword,
            fontSize = 16.sp,
            fontFamily = SCDreamFontFamily,
            modifier = Modifier.weight(1f)
        )
        
        // 변동 표시
        when (item.trend) {
            "UP" -> Icon(
                imageVector = Icons.Default.ArrowUpward,
                contentDescription = "상승",
                tint = Primary500,
                modifier = Modifier.size(16.dp)
            )
            "DOWN" -> Icon(
                imageVector = Icons.Default.ArrowDownward,
                contentDescription = "하락",
                tint = Secondary500,
                modifier = Modifier.size(16.dp)
            )
            else -> Box(
                modifier = Modifier
                    .width(16.dp)
                    .height(2.dp)
                    .background(Color.Gray)
            )
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