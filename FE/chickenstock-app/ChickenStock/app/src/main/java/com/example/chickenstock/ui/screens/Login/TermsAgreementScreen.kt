package com.example.chickenstock.ui.screens.login

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.navigation.NavController
import com.example.chickenstock.ui.theme.SCDreamFontFamily
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.KeyboardArrowLeft
import androidx.compose.foundation.clickable
import androidx.compose.material.icons.filled.CheckBox
import androidx.compose.material.icons.filled.CheckBoxOutlineBlank
import androidx.compose.ui.text.SpanStyle
import androidx.compose.ui.text.buildAnnotatedString
import androidx.compose.ui.text.withStyle
import com.example.chickenstock.viewmodel.MainViewModel
import androidx.lifecycle.viewmodel.compose.viewModel

@Composable
fun TermsAgreementScreen(
    navController: NavController,
    viewModel: MainViewModel = viewModel()
) {
    // 하단바와 탑바 숨기기
    LaunchedEffect(Unit) {
        viewModel.setBottomBarVisibility(false)
        viewModel.setTopBarVisibility(false)
    }

    var isAllAgreed by remember { mutableStateOf(false) }
    var isTermsAgreed by remember { mutableStateOf(false) }
    var isPrivacyAgreed by remember { mutableStateOf(false) }
    
    // 경고 대화상자 표시 상태
    var showExitDialog by remember { mutableStateOf(false) }
    
    // 사용자 입력이 있는지 확인하는 함수
    fun hasUserInput(): Boolean {
        return isTermsAgreed || isPrivacyAgreed
    }
    
    // 뒤로가기 처리
    fun handleBackNavigation() {
        if (hasUserInput()) {
            showExitDialog = true
        } else {
            navController.navigateUp()
        }
    }
    
    // 전체 동의 상태 업데이트
    LaunchedEffect(isTermsAgreed, isPrivacyAgreed) {
        isAllAgreed = isTermsAgreed && isPrivacyAgreed
    }
    
    // 전체 동의 체크박스 클릭 시
    fun onAllAgreedClick() {
        isAllAgreed = !isAllAgreed
        isTermsAgreed = isAllAgreed
        isPrivacyAgreed = isAllAgreed
    }
    
    // 경고 대화상자
    if (showExitDialog) {
        AlertDialog(
            onDismissRequest = { showExitDialog = false },
            title = { Text("동의 취소", fontFamily = SCDreamFontFamily, color = Color.Black, fontWeight = FontWeight.Bold) },
            text = { Text("입력된 정보가 모두 초기화됩니다. 정말 나가시겠습니까?", fontFamily = SCDreamFontFamily, color = Color.Black) },
            confirmButton = {
                TextButton(
                    onClick = {
                        showExitDialog = false
                        navController.navigateUp()
                    }
                ) {
                    Text("나가기", color = Color.Red, fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            dismissButton = {
                TextButton(
                    onClick = { showExitDialog = false }
                ) {
                    Text("계속 작성하기", color = Color(0xFF0066CC), fontFamily = SCDreamFontFamily, fontWeight = FontWeight.Bold)
                }
            },
            containerColor = Color.White
        )
    }
    
    Box(
        modifier = Modifier
            .fillMaxSize()
            .background(Color(0xFFF5F5F5))
    ) {
        // 뒤로가기 버튼
        IconButton(
            onClick = { handleBackNavigation() },
            modifier = Modifier
                .align(Alignment.TopStart)
                .padding(16.dp)
        ) {
            Icon(
                imageVector = Icons.Filled.KeyboardArrowLeft,
                contentDescription = "뒤로가기",
                modifier = Modifier.size(32.dp),
                tint = Color.Black
            )
        }
        
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 24.dp)
                .align(Alignment.Center),
            horizontalAlignment = Alignment.CenterHorizontally,
            verticalArrangement = Arrangement.Bottom
        ) {
            // 제목
            Text(
                text = "약관 동의",
                fontSize = 24.sp,
                fontWeight = FontWeight.Bold,
                fontFamily = SCDreamFontFamily,
                textAlign = TextAlign.Center,
                modifier = Modifier.padding(bottom = 32.dp)
            )
            
            // 전체 동의 체크박스
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = 24.dp)
                    .clickable { onAllAgreedClick() },
                verticalAlignment = Alignment.CenterVertically
            ) {
                Icon(
                    imageVector = if (isAllAgreed) Icons.Filled.CheckBox else Icons.Filled.CheckBoxOutlineBlank,
                    contentDescription = "전체 동의",
                    tint = if (isAllAgreed) Color(0xFFFFEB3B) else Color.Gray,
                    modifier = Modifier.size(24.dp)
                )
                Spacer(modifier = Modifier.width(8.dp))
                Text(
                    text = "전체 동의",
                    fontSize = 18.sp,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black
                )
            }
            
            Divider(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = 24.dp),
                color = Color.LightGray
            )
            
            // 이용약관 동의
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = 16.dp)
                    .clickable { isTermsAgreed = !isTermsAgreed },
                verticalAlignment = Alignment.CenterVertically
            ) {
                Icon(
                    imageVector = if (isTermsAgreed) Icons.Filled.CheckBox else Icons.Filled.CheckBoxOutlineBlank,
                    contentDescription = "이용약관 동의",
                    tint = if (isTermsAgreed) Color(0xFFFFEB3B) else Color.Gray,
                    modifier = Modifier.size(24.dp)
                )
                Spacer(modifier = Modifier.width(8.dp))
                Text(
                    text = buildAnnotatedString {
                        append("이용약관 동의 ")
                        withStyle(SpanStyle(color = Color.Red)) {
                            append("(필수)")
                        }
                    },
                    fontSize = 16.sp,
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black
                )
            }
            
            // 이용약관 내용
            Box(
                modifier = Modifier
                    .fillMaxWidth()
                    .height(100.dp)
                    .background(Color(0xFFF5F5F5))
                    .padding(16.dp)
                    .verticalScroll(rememberScrollState())
            ) {
                Text(
                    text = """
                        제1조 (목적)
                        이 약관은 치킨스톡(이하 "회사")이 제공하는 서비스의 이용과 관련하여 회사와 회원 간의 권리, 의무 및 책임사항을 규정함을 목적으로 합니다.
                        
                        제2조 (정의)
                        1. "서비스"란 회사가 제공하는 모든 서비스를 의미합니다.
                        2. "회원"이란 회사와 서비스 이용계약을 체결한 자를 말합니다.
                        
                        제3조 (서비스의 제공)
                        회사는 회원에게 아래와 같은 서비스를 제공합니다.
                        1. 주식 시뮬레이션 서비스
                        2. 투자 정보 제공 서비스
                        3. 기타 회사가 정하는 서비스
                    """.trimIndent(),
                    fontSize = 14.sp,
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black,
                    lineHeight = 20.sp
                )
            }
            
            Spacer(modifier = Modifier.height(16.dp))
            
            // 개인정보 수집 및 이용 동의
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .padding(bottom = 16.dp)
                    .clickable { isPrivacyAgreed = !isPrivacyAgreed },
                verticalAlignment = Alignment.CenterVertically
            ) {
                Icon(
                    imageVector = if (isPrivacyAgreed) Icons.Filled.CheckBox else Icons.Filled.CheckBoxOutlineBlank,
                    contentDescription = "개인정보 수집 및 이용 동의",
                    tint = if (isPrivacyAgreed) Color(0xFFFFEB3B) else Color.Gray,
                    modifier = Modifier.size(24.dp)
                )
                Spacer(modifier = Modifier.width(8.dp))
                Text(
                    text = buildAnnotatedString {
                        append("개인정보 수집 및 이용 동의 ")
                        withStyle(SpanStyle(color = Color.Red)) {
                            append("(필수)")
                        }
                    },
                    fontSize = 16.sp,
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black
                )
            }
            
            // 개인정보 수집 및 이용 내용
            Box(
                modifier = Modifier
                    .fillMaxWidth()
                    .height(100.dp)
                    .background(Color(0xFFF5F5F5))
                    .padding(16.dp)
                    .verticalScroll(rememberScrollState())
            ) {
                Text(
                    text = """
                        [개인정보 수집 및 이용 목적]
                        - 회원 가입 및 관리
                        - 서비스 제공 및 운영
                        - 고객 문의 및 불만 처리
                        
                        [수집하는 개인정보 항목]
                        - 필수항목: 이메일, 비밀번호, 이름, 닉네임
                        
                        [개인정보 보유 및 이용기간]
                        - 회원 탈퇴 시까지 (단, 관계법령에 따라 보존할 필요가 있는 경우 해당 기간 동안 보관)
                    """.trimIndent(),
                    fontSize = 14.sp,
                    fontFamily = SCDreamFontFamily,
                    color = Color.Black,
                    lineHeight = 20.sp
                )
            }
            Spacer(modifier = Modifier.height(32.dp))
            Button(
                onClick = { 
                    if (isTermsAgreed && isPrivacyAgreed) {
                        navController.navigate("signup")
                    }
                },
                colors = ButtonDefaults.buttonColors(
                    containerColor = if (isTermsAgreed && isPrivacyAgreed) Color(0xFFFFEB3B) else Color.LightGray
                ),
                modifier = Modifier
                    .fillMaxWidth()
                    .height(50.dp),
                shape = MaterialTheme.shapes.medium,
                enabled = isTermsAgreed && isPrivacyAgreed
            ) {
                Text(
                    "다음",
                    color = Color.Black,
                    fontWeight = FontWeight.Bold,
                    fontFamily = SCDreamFontFamily,
                    fontSize = 18.sp
                )
            }
        }
    }
} 