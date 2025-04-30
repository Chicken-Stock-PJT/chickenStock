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
import com.example.chickenstock.data.StockRepository
import com.example.chickenstock.data.Stock
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.focus.onFocusChanged
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.zIndex
import kotlinx.coroutines.flow.collect
import androidx.compose.ui.platform.LocalContext
import android.content.Context
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken

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
    val context = LocalContext.current
    val backStackEntry = navController.currentBackStackEntry
    var searchQuery by remember { mutableStateOf("") }
    
    // SharedPreferences에서 최근 검색어 불러오기
    val prefs = context.getSharedPreferences("recent_searches", Context.MODE_PRIVATE)
    val gson = Gson()
    
    // 최근 검색어 상태 관리
    var recentSearches by remember { 
        mutableStateOf(
            try {
                val json = prefs.getString("searches", "[]")
                gson.fromJson<List<Stock>>(json, object : TypeToken<List<Stock>>() {}.type) ?: emptyList()
            } catch (e: Exception) {
                emptyList()
            }
        )
    }

    // 최근 검색어 저장 함수
    fun saveRecentSearches(searches: List<Stock>) {
        prefs.edit().apply {
            putString("searches", gson.toJson(searches))
            apply()
        }
    }
    
    // savedStateHandle에서 검색어를 실시간으로 가져오기
    LaunchedEffect(backStackEntry?.savedStateHandle) {
        backStackEntry?.savedStateHandle?.getStateFlow<String>("searchQuery", "")?.collect { query ->
            searchQuery = query
        }
    }
    
    // 검색어 자동완성 결과
    val autoCompleteResults = remember(searchQuery) {
        if (searchQuery.isBlank()) {
            emptyList()
        } else {
            StockRepository.searchStocks(searchQuery)
        }
    }

    Column(
        modifier = Modifier
            .fillMaxSize()
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
                recentSearches.forEach { stock ->
                    RecentSearchChip(
                        keyword = stock.shortName,
                        onRemove = {
                            recentSearches = recentSearches.filter { it.shortCode != stock.shortCode }
                            saveRecentSearches(recentSearches)
                        },
                        onClick = {
                            navController.navigate("stock_detail/${stock.shortCode}")
                        }
                    )
                }
            }
        }

        // 검색 결과 표시
        if (searchQuery.isNotBlank()) {
            Spacer(modifier = Modifier.height(24.dp))
            
            Text(
                text = "검색 결과",
                fontSize = 18.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily,
                modifier = Modifier.padding(bottom = 12.dp)
            )
            
            if (autoCompleteResults.isEmpty()) {
                Text(
                    text = "검색 결과가 없습니다.",
                    color = Gray500,
                    fontFamily = SCDreamFontFamily,
                    fontSize = 14.sp,
                    modifier = Modifier.padding(vertical = 16.dp)
                )
            } else {
                LazyColumn {
                    items(autoCompleteResults) { stock ->
                        AutoCompleteItem(
                            stock = stock,
                            onClick = {
                                // 최근 검색어에 추가
                                if (!recentSearches.any { it.shortCode == stock.shortCode }) {
                                    val newSearches = (listOf(stock) + recentSearches).take(5)
                                    recentSearches = newSearches
                                    saveRecentSearches(newSearches)
                                }
                                // 상세 페이지로 이동 (종목 코드만 사용)
                                navController.navigate("stock_detail/${stock.shortCode}")
                            }
                        )
                    }
                }
            }
        }
    }
}

@Composable
fun RecentSearchChip(
    keyword: String,
    onRemove: () -> Unit,
    onClick: () -> Unit
) {
    Surface(
        shape = RoundedCornerShape(16.dp),
        color = Color(0xFFFFF8D6),
        modifier = Modifier
            .height(32.dp)
            .clickable { onClick() }
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
fun AutoCompleteItem(
    stock: Stock,
    onClick: () -> Unit
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .clickable { onClick() }
            .padding(vertical = 12.dp, horizontal = 16.dp),
        horizontalArrangement = Arrangement.SpaceBetween,
        verticalAlignment = Alignment.CenterVertically
    ) {
        Column {
            Text(
                text = stock.shortName,
                fontSize = 16.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily
            )
            Text(
                text = stock.shortCode,
                fontSize = 14.sp,
                color = Gray500,
                fontFamily = SCDreamFontFamily
            )
        }
        Text(
            text = stock.market,
            fontSize = 16.sp,
            fontWeight = FontWeight.Bold,
            fontFamily = SCDreamFontFamily
        )
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