package com.example.chickenstock.ui.screens.ranking

import androidx.compose.foundation.layout.*
import androidx.compose.material3.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import androidx.compose.ui.graphics.Color
import androidx.compose.runtime.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.background
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp
import androidx.compose.ui.text.style.TextAlign
import com.example.chickenstock.api.RankingService
import com.example.chickenstock.api.RetrofitClient
import com.example.chickenstock.api.TotalAssetRankingResponse
import com.example.chickenstock.api.RankingUser
import androidx.compose.ui.platform.LocalContext
import kotlinx.coroutines.launch
import androidx.compose.ui.res.painterResource
import androidx.compose.foundation.Image
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.foundation.border
import androidx.compose.material3.OutlinedButton
import androidx.compose.foundation.clickable

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun RankingScreen(
    navController: NavController
) {
    val context = LocalContext.current
    val rankingService = remember { RetrofitClient.getInstance(context).create(RankingService::class.java) }
    val aiNicknames = listOf("미니 AI", "chill~AI", "쿨한 AI", "귀요미 AI")
    var rankings by remember { mutableStateOf<List<RankingUser>>(emptyList()) }
    var myRank by remember { mutableStateOf<RankingUser?>(null) }
    var isLoading by remember { mutableStateOf(false) }
    var error by remember { mutableStateOf<String?>(null) }
    var isAiRanking by remember { mutableStateOf(false) }
    var isReturnRateRanking by remember { mutableStateOf(false) }
    val token = remember { com.example.chickenstock.data.TokenManager.getInstance(context).getAccessToken() }

    LaunchedEffect(isAiRanking, isReturnRateRanking) {
        isLoading = true
        error = null
        myRank = null
        try {
            if (isAiRanking && isReturnRateRanking) {
                val response = rankingService.getAiReturnRateRanking()
                if (response.isSuccessful) {
                    val aiList = response.body() ?: emptyList()
                    rankings = aiList.map { RankingUser(it.rank, it.nickname, it.returnRate, it.memberId) }
                } else {
                    error = "AI 수익률 랭킹 정보를 불러오지 못했습니다. (${response.code()})"
                }
            } else if (isAiRanking) {
                val response = rankingService.getAiTotalAssetRanking()
                if (response.isSuccessful) {
                    rankings = response.body() ?: emptyList()
                } else {
                    error = "AI 랭킹 정보를 불러오지 못했습니다. (${response.code()})"
                }
            } else if (isReturnRateRanking) {
                val response = if (token.isNullOrBlank()) {
                    rankingService.getReturnRateRanking()
                } else {
                    rankingService.getReturnRateRanking("Bearer $token")
                }
                if (response.isSuccessful) {
                    response.body()?.let {
                        rankings = it.topRankings.map { user ->
                            RankingUser(user.rank, user.nickname, user.returnRate, user.memberId)
                        }
                        myRank = it.myRank?.let { user -> RankingUser(user.rank, user.nickname, user.returnRate, user.memberId) }
                    } ?: run {
                        error = "데이터가 없습니다."
                    }
                } else {
                    error = "수익률 랭킹 정보를 불러오지 못했습니다. (${response.code()})"
                }
            } else {
                val response = if (token.isNullOrBlank()) {
                    rankingService.getTotalAssetRanking()
                } else {
                    rankingService.getTotalAssetRanking("Bearer $token")
                }
                if (response.isSuccessful) {
                    response.body()?.let {
                        rankings = it.topRankings.map { user ->
                            RankingUser(user.rank, user.nickname, user.totalAsset.toDouble(), user.memberId)
                        }
                        myRank = it.myRank?.let { user -> RankingUser(user.rank, user.nickname, user.totalAsset.toDouble(), user.memberId) }
                    } ?: run {
                        error = "데이터가 없습니다."
                    }
                } else {
                    error = "랭킹 정보를 불러오지 못했습니다. (${response.code()})"
                }
            }
        } catch (e: Exception) {
            error = e.message ?: "알 수 없는 오류가 발생했습니다."
        } finally {
            isLoading = false
        }
    }

    Scaffold(
        containerColor = Color(0xFFF5F5F5)
    ) { innerPadding ->
        Box(
            modifier = Modifier
                .fillMaxSize()
        ) {
            if (isLoading) {
                Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
                    CircularProgressIndicator(color = Color(0xFFFFEB3B))
                }
            } else if (error != null) {
                Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
                    Text(error!!, color = Color.Red)
                }
            } else {
                Column(
                    modifier = Modifier.fillMaxSize(),
                    verticalArrangement = Arrangement.SpaceBetween
                ) {
                    // 상단 고정 버튼 (총자산/수익률, AI)
                    Row(
                        modifier = Modifier
                            .fillMaxWidth()
                            .padding(top = 4.dp, start = 20.dp, end = 20.dp, bottom = 8.dp),
                        horizontalArrangement = Arrangement.End
                    ) {
                        OutlinedButton(
                            onClick = { isReturnRateRanking = false },
                            shape = RoundedCornerShape(12.dp),
                            border = BorderStroke(1.dp, if (!isReturnRateRanking) Color(0xFF1976D2) else Color(0xFFB0BEC5)),
                            contentPadding = PaddingValues(horizontal = 12.dp, vertical = 6.dp),
                            colors = ButtonDefaults.outlinedButtonColors(
                                containerColor = if (!isReturnRateRanking) Color(0xFFB3E5FC) else Color(0xFFE3F2FD)
                            )
                        ) {
                            Text(
                                text = "총 자산",
                                fontWeight = FontWeight.Bold,
                                fontSize = 15.sp,
                                color = Color.Black
                            )
                        }
                        Spacer(modifier = Modifier.width(8.dp))
                        OutlinedButton(
                            onClick = { isReturnRateRanking = true },
                            shape = RoundedCornerShape(12.dp),
                            border = BorderStroke(1.dp, if (isReturnRateRanking) Color(0xFF1976D2) else Color(0xFFB0BEC5)),
                            contentPadding = PaddingValues(horizontal = 12.dp, vertical = 6.dp),
                            colors = ButtonDefaults.outlinedButtonColors(
                                containerColor = if (isReturnRateRanking) Color(0xFFB3E5FC) else Color(0xFFE3F2FD)
                            )
                        ) {
                            Text(
                                text = "수익률",
                                fontWeight = FontWeight.Bold,
                                fontSize = 15.sp,
                                color = Color.Black
                            )
                        }
                        Spacer(modifier = Modifier.width(8.dp))
                        OutlinedButton(
                            onClick = { isAiRanking = !isAiRanking },
                            shape = RoundedCornerShape(12.dp),
                            border = BorderStroke(1.dp, Color(0xFF1976D2)),
                            contentPadding = PaddingValues(horizontal = 16.dp, vertical = 6.dp),
                            colors = ButtonDefaults.outlinedButtonColors(
                                containerColor = if (isAiRanking) Color(0xFFB3E5FC) else Color(0xFFE3F2FD)
                            )
                        ) {
                            Image(
                                painter = painterResource(id = com.example.chickenstock.R.drawable.ai),
                                contentDescription = "AI",
                                modifier = Modifier.size(20.dp)
                            )
                            Spacer(modifier = Modifier.width(6.dp))
                            Text(
                                text = "랭킹",
                                fontWeight = FontWeight.Bold,
                                fontSize = 15.sp,
                                color = Color.Black
                            )
                        }
                    }
                    // 랭킹 리스트(100위까지)
                    LazyColumn(
                        modifier = Modifier.weight(1f),
                        contentPadding = PaddingValues(vertical = 12.dp, horizontal = 16.dp)
                    ) {
                        items(rankings) { user ->
                            val isAI = aiNicknames.contains(user.nickname)
                            Surface(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(vertical = 4.dp)
                                    .clickable {
                                        if (isAI) {
                                            navController.navigate("ai_dashboard?nickname=${user.nickname}")
                                        } else {
                                            navController.navigate("ai_dashboard?memberId=${user.memberId}&nickname=${user.nickname}")
                                        }
                                    },
                                shape = RoundedCornerShape(16.dp),
                                color = Color.White,
                                shadowElevation = 2.dp,
                                border = BorderStroke(1.dp, Color(0xFFF0F0F0))
                            ) {
                                Row(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .padding(vertical = 8.dp, horizontal = 8.dp),
                                    verticalAlignment = Alignment.CenterVertically
                                ) {
                                    if (user.rank == 1) {
                                        Image(
                                            painter = painterResource(id = com.example.chickenstock.R.drawable.first),
                                            contentDescription = "1등",
                                            modifier = Modifier.size(36.dp)
                                        )
                                    } else if (user.rank == 2) {
                                        Image(
                                            painter = painterResource(id = com.example.chickenstock.R.drawable.second),
                                            contentDescription = "2등",
                                            modifier = Modifier.size(36.dp)
                                        )
                                    } else if (user.rank == 3) {
                                        Image(
                                            painter = painterResource(id = com.example.chickenstock.R.drawable.third),
                                            contentDescription = "3등",
                                            modifier = Modifier.size(36.dp)
                                        )
                                    } else {
                                        Box(
                                            modifier = Modifier
                                                .size(36.dp)
                                                .background(Color.White, shape = CircleShape)
                                                .border(1.dp, Color(0xFFE0E0E0), shape = CircleShape),
                                            contentAlignment = Alignment.Center
                                        ) {
                                            Text(
                                                text = "${user.rank}",
                                                fontWeight = FontWeight.Bold,
                                                fontSize = 16.sp,
                                                color = if (isAI) Color(0xFF1976D2) else Color.Black
                                            )
                                        }
                                    }
                                    Spacer(modifier = Modifier.width(12.dp))
                                    Row(
                                        modifier = Modifier.weight(1f),
                                        verticalAlignment = Alignment.CenterVertically
                                    ) {
                                        Text(
                                            text = user.nickname,
                                            fontWeight = if (isAI) FontWeight.Bold else FontWeight.Normal,
                                            fontSize = 16.sp,
                                            color = if (isAI) Color(0xFF1976D2) else Color.Black
                                        )
                                        if (isAI) {
                                            Spacer(modifier = Modifier.width(8.dp))
                                            Image(
                                                painter = painterResource(id = com.example.chickenstock.R.drawable.ai),
                                                contentDescription = "AI",
                                                modifier = Modifier.size(22.dp)
                                            )
                                        }
                                    }
                                    Text(
                                        text = if (isReturnRateRanking) String.format("%.2f%%", user.totalAsset) else String.format("%,d원", user.totalAsset.toLong()),
                                        fontWeight = FontWeight.Bold,
                                        fontSize = 16.sp,
                                        color = Color.Black,
                                        textAlign = TextAlign.End,
                                        modifier = Modifier.width(120.dp)
                                    )
                                }
                            }
                        }
                    }
                    // 내 등수(고정) - AI 랭킹 모드에서는 표시하지 않음
                    if (!isAiRanking && myRank != null) {
                        Surface(
                            modifier = Modifier
                                .fillMaxWidth()
                                .padding(start = 8.dp, end = 8.dp, bottom = 8.dp, top = 0.dp),
                            shape = RoundedCornerShape(16.dp),
                            color = Color.White,
                            shadowElevation = 2.dp,
                            border = BorderStroke(1.dp, Color(0xFFF0F0F0))
                        ) {
                        Row(
                            modifier = Modifier
                                .fillMaxWidth()
                                .background(Color(0xFFFFF9C4))
                                .padding(vertical = 18.dp, horizontal = 20.dp),
                            verticalAlignment = Alignment.CenterVertically
                        ) {
                            if (myRank!!.rank == 1) {
                                Image(
                                    painter = painterResource(id = com.example.chickenstock.R.drawable.first),
                                    contentDescription = "1등",
                                    modifier = Modifier.size(36.dp)
                                )
                            } else if (myRank!!.rank == 2) {
                                Image(
                                    painter = painterResource(id = com.example.chickenstock.R.drawable.second),
                                    contentDescription = "2등",
                                    modifier = Modifier.size(36.dp)
                                )
                            } else if (myRank!!.rank == 3) {
                                Image(
                                    painter = painterResource(id = com.example.chickenstock.R.drawable.third),
                                    contentDescription = "3등",
                                    modifier = Modifier.size(36.dp)
                                )
                            } else {
                                Box(
                                    modifier = Modifier
                                        .size(36.dp)
                                        .background(Color.White, shape = CircleShape)
                                        .border(1.dp, Color(0xFFE0E0E0), shape = CircleShape),
                                    contentAlignment = Alignment.Center
                                ) {
                                    Text(
                                        text = "${myRank!!.rank}",
                                        fontWeight = FontWeight.Bold,
                                        fontSize = 16.sp,
                                        color = Color(0xFFE74C3C)
                                    )
                                }
                            }
                            Spacer(modifier = Modifier.width(12.dp))
                            Text(
                                text = myRank!!.nickname,
                                fontWeight = FontWeight.Bold,
                                fontSize = 16.sp,
                                color = Color(0xFFE74C3C),
                                modifier = Modifier.weight(1f)
                            )
                            Text(
                                text = if (isReturnRateRanking) String.format("%.2f%%", myRank!!.totalAsset) else String.format("%,d원", myRank!!.totalAsset.toLong()),
                                fontWeight = FontWeight.Bold,
                                fontSize = 16.sp,
                                color = Color(0xFFE74C3C),
                                textAlign = TextAlign.End,
                                modifier = Modifier.width(120.dp)
                            )
                        }
                        }
                    }
                }
            }
        }
    }
} 