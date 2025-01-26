# PhD_Lectures_Macerata_2025

This repository includes material prepared for my lectures at the [PhD in Quantitative Methods for Economic Policy](https://economiaediritto.unimc.it/en/research/phd-programme-and-research-grants/PhD-MQPE), University of Macerata, February 3-4, 2025.

**WARNING: WORK IN PROGRESS** :wrench:

## Table of Contents

- [A1 Introduction to endogenous money](A1_Introduction_to_Endogenous_Money)
- [A2 Presuppositions of the TMC](A2_Presuppositions)
- [A3 A simple TMC model](A3_A_Simple_TMC_Model)
- [A4 On some misconceptions surrounding the TMC](A4_On_some_misconceptions)
- [A5 Implementing a TMC model in R](A4_Implementing_a_SFC_Model_in_R)
- [B1 Introduction to SFC models](#B1_Introduction)
- [B2 Model PC](#2_Model_PC)
- [B3 Model IO-PC](#3_Model_IO-PC)
- [B4 Model ECO-IO-PC](#4_Model_ECO-IO-PC)
- [B5 Experiments](#5_Experiments)
- [B6 Suggested readings](#6_Suggested_readings)


## A1_Introduction_to_Endogenous_Money

The (Italian) [Theory of the Monetary Circuit](https://en.wikipedia.org/wiki/Monetary_circuit_theory) (TMC) is the culmination of a research line initiated by Augusto Graziani in the late 1970s, but only fully developed during the following decade. The TMC offers an original reinterpretation of some of the most significant aspects of Karl Marx's analysis, which resembles heterodox monetary views within the history of 20th-century economic thought.

This perspective draws on a subterranean strand of studies originating from Knut Wicksell's *Interest and Prices* (1898), progressing through the Swedish School and the works of Hahn and Robertson, and culminating in Schumpeter's monetary works and Keynes's *Treatise on Money* (1930). However, it is primarily to the works of certain French authors, notably Jaques Le Bourva and especially Bernard Schmitt, that the first attempt to construct a general macro-monetary theory, as an alternative and rival to traditional doctrine, can be attributed.

It is no secret that the authors cited differ, sometimes radically, in their approaches. Nonetheless, Graziani's research programme demonstrates that divergences are less significant than the shared aspects. The latter, if carefully interpreted and integrated, are sufficient to form a coherent theoretical framework that constitutes the embryonic stage of a monetary theory of capitalist production.

A foundational analytical premise of this framework is the assimilation of Keynes's category of 'finance' with Marx's concept of 'money capital', which assigns a central role to the relationship between money and production within the economic process. This approach enables a depiction of the functioning of capitalist market economies as a circular sequence of interconnected phases. The sequence begins with the granting of bank credit to non-financial firms, which provides them with the purchasing power needed to acquire labour for production. The monetary circuit is only completed when firms realise the value of their production in monetary terms, repay their debts to banks, and distribute the net product between profits and interest.

The methodological and theoretical presuppositions of the TMC include:

1. the rejection of so-called methodological individualism in favour of an analysis that explicitly considers the hierarchy and differing roles of economic agents;

1. the adoption of a sequential analytical approach, in contrast to the neoclassical model of simultaneous equilibrium;
  
1. the principle of the endogenous determination of the money within the economic system;
  
1. lastly, the rejection of the neoclassical theory of income distribution.

Based on these presuppositions, the main purpose of the TMC is to identify the logical phases of the process of creation, destruction and circulation of money in a capitalist economy, inhabited by different and opposed social classes.


## A2_Presuppositions

According to Graziani (2006, pp. 25-26) the main theoretical presuppositions of the TMC are:

1. Money is in the nature of credit money (bank credit).

1. Credit money is created whenever an agent spends money granted to her by a bank and is destroyed whenever a bank credit is repaid.

1. Money is an endogenous flow: it is created and injected inot the economy by means of negotiations between banks and firms.

1. Class divide matters: since firms have access to bank credit at *the beginning of the circuit*, they enjoy a purchasing power that is not constrained by the level of real income (or wealth). By constrast, wage earners can only spend alread-yearned income.

1. Since agents are not granted credit on the same footing, the system of relative prices reflects the way in which purchasing power has been granted to different social groups. As a consequence, money is never neutral.

1. A complete theoretical analysis must explain the whole itinerary followed by money: creation, circulation, destruction. Money existence and operation can be described as a 'circuit'.

1. The creation of money determines both the division of production between consumption and investment goods and the distribution of income between wages and profits. Therefore, the role of money goes far beyond that of lubricant of echanges (and store of value). To the firms, the initial access to money is the source of profits.


## A3_A_Simple_TMC_Model

The the simplest version of the monetary circuit, only four agents are considered: the central bank, commercial banks, production firms, and wage earners.

:bank: **Step 1: money creation**. Commercial banks to grant credit to firms, thus enabling them to start the process of production. This credit flow is denominated *initial finance*. Only frms are admitted to bank credit *at this stage* (we will discuss thoroughly this point later on). If firms are considered as one integrated and consolidated sector, the only purchase they have to make is that of labour-force. Therefore, their only payment is the wage bill. As a result, the wage policy of the firms ultimately depends on the credit policy of the banks.

## Firms

| Description       | Amount |
|-------------------|--------|
| Deposits (A)      | -1€    |

## Banks

| Description       | Amount |
|-------------------|--------|
| Deposits (L)      | -1€    |
| Deposits (L)      | -1€    |

## Wage earners

| Description       | Amount |
|-------------------|--------|
| Deposits (A)      | +1€    |



:factory: **Step 2: production and money circulation**. Firms autonomously decide both the employment level and the amount of consumer goods and investment goods produced. Wage earners can only decide how to distribute their money
income between consumption and saving, and how to allocate their saving, which can take the form of either bank deposits or securities (here only private securities).

:hamburger: **Step 3: sales on the goods market**. Goods produced are put on sale. Consumer goods are sold to wage earners, while investment goods are exchanged inside the firms sector. The money that wage earners spend in the goods
market flows back to the firms.

:chart_with_upwards_trend: **Step 4: portfolio investment**. The money that wage earners spend in the financial market on the purchase of securities flows back to the firms.

:bank: **Step 5: money destruction**. Firms use the money spent by the wage earners on the markets to repay their bank debt. To the extent that bank debts are repaid, an equal amount of money is destroyed. To the extent instead that wage earners use their money to increase their own deposit holdings (or cash balances), an equal amount of money remains in existence in the form of firms' debt and wage earners' credit towards the banks.

...

<figure>
<img
src="https://github.com/marcoverpas/figures/blob/main/circuit_scheme.jpg" width="800">
</figure>


## A4_On_some_misconceptions

...


## A5_Implementing_a_SFC_Model_in_R

... (stress implications of economic policy) (create interactive models)



## B1_Introduction_to_SFC_Models

In the last three decades, macroeconomic modelling has been dominated by Dynamic Stochastic General Equilibrium (DSGE) models. However, dissatisfaction with these models has been growing since the mid-2000s. Three primary weaknesses have been identified in DSGE models: unrealistic assumptions, a limited range of considerations, and poor data fit.

In response to these shortcomings, alternative models have emerged, aiming to address specific deficiencies. Computable General Equilibrium (CGE) models allow for broadening the scope but usually share key neoclassical assumptions with DSGE models. Leontief-like Input-Output (IO) models provide more room for a sound analysis of cross-industry interdependencies and the role of aggregate demand, but they still face limitations (as these models are static in nature, and their coverage of financial aspects is usually quite limited). Heterogeneous agent-based models, network analysis, and other complexity models offer novel perspectives. Lastly, Stock-Flow Consistent (SFC) models have gained traction, finding applications in empirical macroeconomic research by institutions like the [Bank of England](https://www.bankofengland.co.uk/-/media/boe/files/working-paper/2016/a-dynamic-model-of-financial-balances-for-the-uk) and the [Italian Ministry of Economy and Finance](https://www.sciencedirect.com/science/article/abs/pii/S0264999322003509).

The resurgence of interest in SFC models can be traced back to Wynne Godley's [successful predictions](https://www.levyinstitute.org/publications/seven-unsustainable-processes) of the U.S. crises in 2001 and 2007. Godley's work laid the foundation for modern SFC modelling. These models, rooted in national accounts and flow of funds, integrate financial and real aspects of the economy, allowing for the identification of unsustainable processes.

SFC models are akin to 'system dynamics' models, analysing complex systems over time, tracking flows, stocks, and utilising feedback loops. Basic SFC models have evolved into various forms, including Open-Economy or Multi-Area SFC models (MA-SFC), Ecological SFC models (ECO-SFC), Interacting Heterogeneous Agent-Based SFC models (AB-SFC), Input-output SFC Models (IO-SFC), and Empirical SFC Models (E-SFC).

Crucially, SFC models adhere to four accounting principles: flow consistency, stock consistency, stock-flow consistency, and quadruple book-keeping. The economy is represented by accounting matrices, identities reflecting the System of National Accounts, and dynamic equations defining the behaviour of each sector (usually based on rules of thumb and stock-flow norms).

Standard SFC models typically fall within the range of medium-scale macro-econometric dynamic models. These models are commonly formulated in discrete time using difference equations. However, continuous-time formulations using differential equations are also employed. 

The simplicity or complexity of SFC models dictates the approach to solving them. The simplest models, featuring a limited number of equations, can be solved analytically by finding steady-state solutions. These solutions provide insights into the long-term behaviour of the model. More advanced SFC models, characterised by increased complexity and a larger number of equations, necessitate computational methods. Computer simulations become essential for understanding the dynamic interactions within the system. Numerical techniques, such as iterative methods, are employed to capture the intricate dynamics of these models.

![fig_lang](https://raw.githubusercontent.com/marcoverpas/figures/main/languages.png)

Coefficients in SFC models play a crucial role in shaping model behaviour. Researchers have several options for determining these coefficients:

a) Fine-tuning: coefficients can be fine-tuned to achieve specific baseline scenarios, drawing insights from previous studies or selecting values from a reasonable range.

b) Calibration: researchers may calibrate coefficients to match the model's predictions with observed data, aligning the model with real-world economic conditions.

c) Estimation: econometric methods, including Ordinary Least Squares (OLS) and cointegration techniques, enable the estimation of coefficients from observed data, enhancing the model's empirical relevance.

Unlike DSGE models, which often rely on a unified platform like `Dynare`, SFC modeling lacks a universally adopted program. The pioneering codes used in Godley and Lavoie's work were developed by [Gennaro Zezza](https://gennaro.zezza.it/software/eviews/gl2006.php) using `EViews` and `Excel`. However, the landscape has evolved over time. [R](https://www.r-project.org/) ([RStudio](https://posit.co/blog/rstudio-new-open-source-ide-for-r/)) has become the predominant programming environment for SFC modeling, owing to its flexibility and extensive capabilities. Dedicated `R` packages such as [SFCR](https://joaomacalos.github.io/sfcr/index.html) and [Godley](https://github.com/gamrot/godley/) provide specialized tools for SFC modeling. Additionally, [Bimets](https://github.com/andrea-luciani) offers a platform for empirical SFC model development. Alternative programming languages, including [MATLAB](https://github.com/marcoverpas/SFC-models-Matlab) (with or without `Dynare`), `Mathematica`, [Python](https://github.com/marcoverpas/SFC-models-Jupyter), and `Julia`, find applications in SFC modeling, particularly for creating agent-based SFC models. [Minsky](https://www.kickstarter.com/projects/2123355930/minsky-reforming-economics-with-visual-monetary-mo), a software package developed by Steve Keen, stands out as a tool for visually modeling macroeconomic system dynamics.

Summing up, while there are many online resources today for those interested in engaging with SFC modeling, the community of SFC modelers lacks a portal that consolidates the produced material, which is abundant but still scattered.

The aim of this set of lectures is to provide a simple introduction to IO- and ECO-SFC models by using a hand-on approach. For a theoretical introduction to SFC model, please refer to [Monetary Economics. An Integrated Approach to Credit, Money, Income, Production and Wealth](https://link.springer.com/book/10.1007/978-1-137-08599-3) by W. Godley and M. Lavoie. 

## B2_Model_PC

This is one of the simplest SFC toy models and will we use it as our benchmark model. It is developed in chapter 4 of "[Monetary Economics. An Integrated Approach to Credit, Money, Income, Production and Wealth](https://link.springer.com/book/10.1007/978-1-137-08599-3)". **PC** stands for "portfolio choice", because households can hold their wealth in terms of cash and/or government bills.

Key assumptions are as follows:

1. Closed economy

1. Four agents: households, “firms”, government, central bank

1. Two financial assets: government bills and outside money (cash)

1. No investment (accumulation) and no inventories

1. Fixed prices and zero net profits

1. No banks, no inside money (bank deposits)

1. No ecosystem

The structure of Model PC is quite simple. Crucial identities of the model are derived using the balance-sheet matrix and the transaction-flow matrix. These tables are also useful to double-check model consistency in each period. 

#### Table 1. Balance sheet matrix  


|              | Households  | Firms      |Central bank |Government | Row. tot |
|:------------:|:------:     |:------:    |:------:     |:------:   |:---:     |
|              |             |            |             |           |          |
|Cash (money)  |$$H_h$$      |            |$$-H_s$$     |           |   0      |
|Bills         |$$B_h$$      |            |$$B_{cb}$$   |$$-B_s$$   |   0      |
|Wealth        |$$-V_h$$     |            |             |$$V_g$$    |   0      |
|              |             |            |             |           |          |
|Column tot.   |   0         |0           |0            |0          |   0      |


#### Table 2. Transactions-flow matrix  

|                       |Households           |Firms             |Central bank             |Government           | Row. tot |
|:----------------------|:---------------:    |:----------------:|:--------------:         |:--------------:     |:-------: |
|                       |                     |                  |                         |                     |          |
|Consumption            |$$-C_d$$             |$$C_s$$           |                         |                     |   0      |
|Government expenditure |                     |$$G$$             |                         |$$-G$$               |   0      |
|GDP (income)           |$$Y$$                |$$-Y$$            |                         |                     |   0      |
|Interest payments      |$$r \cdot B_{h,-1}$$ |                  |$$r \cdot B_{cb,-1}$$    |$$-r \cdot B_{s,-1}$$|   0      |
|CB profit              |                     |                  |$$-r \cdot B_{cb,-1}$$   |$$r \cdot B_{cb,-1}$$|   0      |
|Taxes                  |$$-T$$               |                  |                         |$$T$$                |   0      |
|                       |                     |                  |                         |                     |          |
|Change in cash         |$$-\Delta H_h$$      |                  |$$\Delta H_s$$           |                     |   0      |
|Change in bills        |$$-\Delta B_h$$      |                  |$$-\Delta B_{cb}$$       |$$\Delta B_s$$       |   0      |
|                       |                     |                  |                         |                     |          |
|Column tot.            |0                    |0                 |0                        |0                    |   0      |

*Note*: The code needed to generate **Table 1** and **Table 2** can be accessed [here](https://github.com/marcoverpas/EAEPE_summer_school_2024/blob/main/eaepe_io_tables.R).

Completing the identities derived form the tables above with behavioural equations for taxes, consumption, demand for bills, and the interest rate, we obtain the following system of difference equations:

*Equation (`1`)* - National income (identity): 
$$Y = C + G $$
where $C$ is household consumption and $G$ is government expenditure.

*Equation (`2`)* - Disposable income (identity):
$$YD = Y - T + r_{-1} \cdot B_{h,-1} $$
where $r$ is the interest rate and $B_h$ is households' holdings of bills. The subscript $-1$ stands for lagged variable.

*Equation (`3`)* - Tax revenue (behavioural):
$$T = \theta \cdot (Y +  r_{-1} \cdot B_{h,-1} ) $$
where $\theta$ is the average tax rate on total income before taxes.

*Equation (`4`)* - Household wealth (identity):
$$V_h = V_{h,-1} + YD - C  $$

*Equation (`5`)* - Consumption (hehavioural):
$$C = \alpha_1 \cdot YD + \alpha_2 \cdot V_{-1}  $$
where $\alpha_1$ is the propensity to consume out of income and $\alpha_2$ is the propensity to consume out of wealth.

*Equation (`6`)* - Cash held by households (identity):
$$H_h = V_h - B_h  $$

*Equation (`7`)* - Bills held by households (behavioural):
$$\frac{B_h}{V_h} = \lambda_0 + \lambda_1 \cdot r - \lambda_2 \cdot \frac{YD}{V_h}  $$
where $\lambda_0$ is the autonomous share of bills held by the households, $\lambda_1$ is the elasticity to the interest rate, and $\lambda_2$ captures households' liquidity preference.

*Equation (`8`)* - Supply of bills (identity):
$$B_s = B_{s,-1} + G - T + r_{-1} \cdot ( B_{s,-1} - B_{cb,-1} )  $$
where $B_{cb}$ is the amount of bills held by the central bank.

*Equation (`9`)* - Supply of cash (identity):
$$H_s = H_{s,-1} + \Delta B_{cb} $$

*Equation (`10`)* - Bills held by central bank (identity):
$$B_{cb} = B_s - B_h $$

*Equation (`11`)* - Interest rate (behavioural):
$$r = \bar{r} $$
where $\bar{r}$ is the policy rate set by the central bank.

Note that any complete and coherent model must contain an equation that is *redundant*, meaning that it is logically implied by all the others (*Walras' Law*). In Model PC, this is the equation matching the demanded stock of cash with central bank's supply of cash:

$$H_s = H_h $$

This equation must be excluded from the model, which is the reason it is sometimes called the *hidden equation*. However, it can be conveniently used to double-check the consistency of the model.

One of the advantages of using such a simple model as a basic model, is that we can calculate the (quasi) steady state solution for national income $`Y`$. We can do that by observing that in the steady state there must be no saving ($`C=YD`$) and household holdings of bills are stable ($`B_{h,−1}=B_h=B_h^{*}`$). Using these conditions and equations (2) and (3) in $`Y`$, we otain: 

$$Y^{\*}= \frac{G + r \cdot B_h^{*} \cdot (1 − \theta)}{\theta}$$

where $`B_h^{*}`$ is the steady-state value of household stock of bills.

The model can be easily simulated by identifying the coefficients and attributing an initial value to government spending ($G=G_0$, with $G_0>0$). A simple `R` code replicating Model PC can be found [here](https://github.com/marcoverpas/Six_lectures_on_sfc_models/blob/main/PC_model_from_scratch.R). 

## B3_Model_IO-PC

There are currently only a few prototype input-output SFC models, despite recent progress (Berg, Hartley, and Richters, 2015; Jackson and Jackson, 2021, 2023; Valdecantos, 2023). Integrating IO and SFC techniques poses challenges, but it is crucial for analysing both technical progress (Veronese Passarella, 2023) and the interaction of the ecosystem with the economy (Hardt and O’Neill, 2017).

Model IO-PC is an input-output extension of Model PC, where **IO** stands for "input-output structure".

In comparison to Model PC, additional assumptions include:

1. Two industries are considered (within the firms sector)

1. Only circulating capital is used

1. Technical coefficients are fixed

1. Each industry uses only one technique to produce one product

1. Prices are determined by reproduction conditions

1. The composition of both consumption and government spending is exogenously defined

It is also assumed that the marginal propensity to consume out of income is a negative function of the interest rate (as an increase in $r$ redistributes income from the wage earners to the rentiers, and the latter are marked by a lower propensity to consume). Based on these assumptions, a few additional equations are required to transform Model PC into Model IO-PC.

Notice that scalars are represented using *italic characters*, whereas vectors and matrices are represented using non-italic characters hereafter.

*Equation (`12`)* - Column vector defining *composition of real consumption* (behavioural):

$$**\text{B}_c** = **\bar{\text{B}}_c**  $$  

where $\mathrm{B_c} = [ B_{c1} \text{ }  B_{c2} ]$ and $B_{c1} + B_{c2} = 1$.

*Equation (`13`)* - Column vector defining *composition of real government expenditure* (behavioural):

$$**\text{B}_g** = **\bar{\text{B}}_g** $$  

where $\mathrm{B_g} = [ B_{g1} \text{ } B_{g2} ]$ and $B_{g1} + B_{g2} = 1$.

*Equation (`14`)* - Column vector of *final demands in real terms* (identity):

$$**\text{d}** = **\text{B}_c** \cdot c + **\text{B}_g** \cdot g $$  

*Equation (`15`)* - Column vector of *real gross outputs* (identity):

$$**\text{x}** = **\text{A}** \cdot **\text{x}** + **\text{d}**, ~ with: **\text{A}**= \left(\begin{array}{cc} a_{11} & a_{12} \\
                                                                        a_{21} & a_{22}
                                                                        \end{array}\right) $$  

*Equation (`1.A`)* - Modified equation for *national income* (identity):

$$Y = **\text{p}^T** \cdot **\text{d}** $$

*Equation (`16`)* - Column vector of *unit prices of reproduction* (behavioural):

$$**\text{p}** = \frac{w}{**\text{pr}**} + ( **\text{p}**^T \cdot **\text{A}** ) \cdot (1 + \mu) $$

where $w$ is the (uniform) wage rate, $\mathrm{pr}$ is the vector of labour productivities, and $\mu$ is the (uniform) profit rate.

*Equation (`17`)* - *Average consumer price* (identity):

$$p_c = **\text{p}^T** \cdot **\text{B}_c** $$  

*Equation (`18`)* - *Average price for the government* (identity):

$$p_g = **\text{p}^T** \cdot **\text{B}_g** $$  

*Equation (`5.A`)* - *Real consumption function* (behavioural):
$$c = \alpha_1 \cdot \left( \frac{YD}{p_c} - \pi \cdot \frac{V_{-1}}{p_c} \right) + \alpha_2 \cdot \frac{V_{-1}}{p_{c,-1}} $$

where $\pi$ is the rate of growth of the consumer price index (inflation rate), as consumers are assumed not to suffer from monetary illusion.

*Equation (`19`)* - *Propensity to consume out of income* (behavioural):
$$\alpha_1 = \alpha_{10} - \alpha_{11} \cdot r_{-1}$$

where $\alpha_{10}$ and $\alpha_{11}$ are positive coefficients.

Note: the superscript $T$ stands for the transpose of the matrix, turning a column vector into a row vector. 

Equations (`12`) to (`19`) are additional ones. Equations (`1.A`) and (`5.A`) replace equations (`1`) and (`5`) of Model PC, respectively. Nominal consumption in equation (`4`) and nominal government spending in equation (`8`) are redefined as $p_c \cdot c$ and $p_g \cdot g$, respectively. The main code for developing Model IO-PC and conducting experiments can be found [here](https://github.com/marcoverpas/EAEPE_summer_school_2024/blob/main/eaepe_io_model.R).

Using the hidden equation, **Figure 1** demonstrates that the model is watertight, while **Figure 2** illustrates that the evolution of the main macro variables towards the steady state exactly matches that of a standard (aggregative) SFC model. However, unlike a standard SFC model, Model IO-PC also allows for the accounting of the input-output structure of the economy.

The input-output matrix of Model IO-PC is shown in **Table 3**.

#### Table 3. Simplified input-output matrix

|                             | Industry 1 (demand)  | Industry 2 (demand)  |  Final demand   |             Output              |
|:----------------------------|:--------------------:|:--------------------:|:---------------:|:-------------------------------:|
|                             |                      |                      |                 |                                 |
| **Inudstry 1 (production)** | \$p_1 x_1 a\_{11} \$ | \$p_1 x_2 a\_{12} \$ | $p_1 \cdot d_1$ |         $p_1 \cdot x_1$         |
| **Industry 2 (production)** | \$p_2 a\_{21} x_1 \$ | \$p_2 a\_{22} x_2 \$ | $p_2 \cdot d_2$ |         $p_2 \cdot x_2$         |
| **Value added**             |        $yn_1$        |        $yn_2$        |      $yn$       |                                 |
| **Output**                  |   $p_1 \cdot x_1$    |   $p_2 \cdot x_2$    |                 | $\mathrm{p}^T \cdot \mathrm{x}$ |

*Note*: The code needed to generate **Table 3** can be accessed [here](https://github.com/marcoverpas/EAEPE_summer_school_2024/blob/main/eaepe_io_tables.R).

**Table 3** illustrates cross-industry interdependencies in a simplified economy where two products are produced using the same products and labour.

These interdependencies are also depicted in the chart below. Specifically, **Figure 3** shows the final demands for each product, while **Figure 4** details the inputs required by each industry to produce their respective outputs.

<figure>
<img
src="https://github.com/marcoverpas/figures/blob/main/io_plot_1.png" width="800">
</figure>

Figures 1 to 4 show the evolution of model variables over time. Sankey diagrams can be used to take snapshots of the economy in a specific period.

<figure>
  <img
  src="https://github.com/marcoverpas/figures/blob/main/sankey_combined.png">
</figure>

For instance, the diagram above illustrates the payments across social sectors and the flows of inputs across industries in period 20. The related additional code can be found [here](https://github.com/marcoverpas/EAEPE_summer_school_2024/blob/main/eaepe_sankey_diagram.R).


## B4_Model_ECO-IO-PC

Although the origins of ecological macroeconomics can be traced back to the inception of economics itself, early SFC models for economic research did not incorporate the ecosystem.

This gap was bridged in the late 2010s (Dafermos, Nikolaidi, and Galanis, 2016, 2018; Jackson and Victor, 2015). The primary characteristic of ecological SFC models is their integration of monetary variables (following Godley and Lavoie, 2008) with physical variables (in line with Georgescu-Roegen, 1971) in a consistent manner. Several ECO-SFC models have been developed since then.

Here we consider a simple extension of Model IO-PC, named Model ECO-IO-PC, where **ECO** stands for "ecological". Additional assumptions are as follows:

1. there are 2 types of reserves: matter and energy

1. there are 2 types of energy: renewable and non-renewable

1. Resources are converted into reserves at a certain rate

1. Industrial CO2 emissions are associated with the use of non-renewable energy

1. Atmospheric temperature is a growing function of CO2 emissions

1. Both goods from industry 1 and industry 2 can be durable or non-durable

1. A share of durable goods (hence socio-economic stock) is discarded in every period

1. Both waste and emissions are produced only by the firm sector

Behavioural equations draw inspiration from the works of [Dafermos, Nikolaidi, and Galanis (2016, 2018)](https://define-model.org/). The main code of Model ECO-IO-PC is available [here](https://github.com/marcoverpas/EAEPE_summer_school_2024/blob/main/eaepe_eco_model.R). New variables and coefficients are defined between line 88 and line 125. The blocks providing the additional equations for the ecosystem are those included between line 252 and 333.

Firstly, extractions of matter and waste are modelled.

*Equation (`20`)* - Production of *material goods*:

$$ x_{mat} = **\text{m}_{mat}^T** \cdot **\text{x}** $$

where $\text{m}_{mat}$ is the vector of material intensity coefficients by industry.

*Equation (`21`)* - *Extraction* of matter:

$$mat = x_{mat} - rec$$

*Equation (`22`)* - *Recycled* matter:

$$rec = \rho_{dis} \cdot dis$$

where $\rho_{dis}$ is the recycling rate of discarded products.

*Equation (`23`)* - *Discarded* socioeconomic stock:  

$$dis = **\text{m}_{mat}^T** \cdot (**\text{z}_{dc}** \odot **\text{dc}_{-1}**)$$

where $\text{z}_{dc}$ is the vector defining the percentages of discarded socio-economic stock by industry.

*Equation (`24`)* - Stock of *durable goods*:

$$ **\text{dc}** = **\text{dc}_{-1}** + **\text{B}_c** \cdot c - **\text{z}_{dc}** \cdot **\text{dc}_{-1}** $$

*Equation (`25`)* - *Socioeconomic stock*:

$$k_h = k_{h,-1} + x_{mat} - dis $$

*Equation (`26`)* - *Waste*:

$$wa = mat - (k_h - k_{h,-1}) $$

Secondly, energy use and CO2 emissions are considered.

*Equation (`27`)* - *Total energy* required for production:

$$en = **\text{e}_{en}^T** \cdot **\text{x}**$$

where $\text{e}_{en}$ is the vector that defines the energy intensity coefficients by industry.

*Equation (`28`)* - *Renewable* energy at the end of the period:

$$ren = **\text{e}_{en}^T** \cdot (**\text{a}_{en}** \odot **\text{x}** )                 $$

where $\text{a}_{en}$ is the vector that defines the industry-specific shares of renewable energy to total energy.

*Equation (`29`)* - *Non-renewable* energy:

$$nen = en - ren$$

*Equation (`30`)* - Annual flow of *CO2 emissions*:

$$emis = \beta_{e} \cdot nen$$

where $\beta_{e}$ is the CO2 intensity coefficient of non-renewable energy sources.

*Equation (`31`)* - *Cumulative* emissions:

$$co2_{cum} = co2_{cum,-1} + emis$$

*Equation (`32`)* - Atmospheric *temperature*:

$$temp = \frac{1}{1-fnc} \cdot tcre \cdot co2_{cum}$$

where $fnc$ is the non-CO2 fraction of total anthropocentric forcing and $tcre$ is the transient climate response to cumulative carbon emissions.

Thirdly, the dynamics of reserves is modelled.

*Equation (`33`)* - Stock of *matter reserves*:

$$k_m = k_{m,-1} + conv_m - mat$$

*Equation (`34`)* - Matter resources *converted* to reserves:

$$conv_m = \sigma_{m} \cdot res_m$$

where $\sigma_{m}$ is the conversion rate of matter resources into reserves.

*Equation (`35`)* - Stock of *matter resources*:

$$res_m = res_{m,-1} - conv_m$$

*Equation (`36`)* - *Carbon mass* of non-renewable energy:

$$cen = \frac{emis}{car}$$

where $car$ is the coefficient converting Gt of carbon into Gt of CO2.

*Equation (`37`)* - Mass of *oxygen*:

$$o2 = emis - cen$$

*Equation (`38`)* - Stock of *energy reserves*:

$$k_e = k_{e,-1} + conv_e - en$$

*Equation (`39`)* - Energy resources *converted* to reserves:

$$conv_e = \sigma_e \cdot res_e$$

where $\sigma_e$ is the conversion rate of energy resources into reserves.

*Equation (`40`)* - Stock of *energy resources*:

$$res_e = res_{e,-1} - conv_e$$

Lastly, feedback effects and damages can be introduced. Here the
assumption is made that the propensity to consume out of income is
(negatively) influenced by climate change.

*Equation (`19.A`)* - New *propensity to consume* out of income:

$$\alpha_1 = \alpha_{10} - \alpha_{11} \cdot r_{-1} - \alpha_{12} \cdot \Delta temp$$

where $\alpha_{12}$ is a positive coefficient.

The related physical constraints (used to define ecosystem identities) are displayed by **Table 4** and **Table 5**, in which matter is expressed in $Gt$ and energy is expressed in $EJ$. 

#### Table 4. Physical flow matrix

|                               |*Matter*        |*Energy*        |
|:------------------------------|:------------:  |:--------------:|
|                               |                |                |
|**INPUTS**                     |                |                |
|Extracted matter               |$$mat$$         |                |
|Recycled socio-economic stock  |$$rec$$         |                |
|Renewable energy               |                |$$ren$$         |
|Non-renewable energy           |$$cen$$         |$$nen$$         |
|Oxygen                         |$$o2$$          |                |
|                               |                |                |
|**OUTPUTS**                    |                |                |
|Industrial CO2 emissions       |$$-emis$$       |                |
|Discarded socio-economic stock |$$-dis$$        |                |
|Dissipated energy              |                |$$-nen$$        |
|Change in socio-economic stock |$$-\Delta k_h $$|                |
|                               |                |                |
|**TOTAL**                      |0               |0               |


#### Table 5. Physical stock-flow matrix

|                                     |*Matter*     |*Energy*     |*CO2*            |*SES*       |
|:------------------------------------|:----------: |:----------: |:----------:     |:----------:|
|                                     |             |             |                 |            |
|**INITIAL STOCK**                    |$$k_{m,-1}$$ |$$k_{e,-1}$$ |$$co2_{cum,-1}$$ |$$k_{h,-1}$$|
|Resources converted into reserves    |$$conv_m$$   |$$conv_e$$   |                 |            |
|CO2 emissions                        |             |             |$$emis $$        |            |
|Production of material goods         |             |             |                 |$$x_{mat} $$|
|Extraction of matter / use of energy |$$-mat$$     |$$-en $$     |                 |            |
|Destruction of socio-economic stock  |             |             |                 |$$-dis$$    |
|**FINAL STOCK**                      |$$k_m$$      |$$k_e$$      |$$co2_{cum}$$    |$$k_h$$     |

*Note*: The code needed to generate **Table 4** and **Table 5** can be accessed [here](https://github.com/marcoverpas/EAEPE_summer_school_2024/blob/main/eaepe_eco_tables.R).

A dynamic rendition of the model is provided in the new **Figure 3** and **Figure 4**, which now show the evolution over time of matter and energy reserves, and the atmospheric temperature, respectively.

<figure>
<img
src="https://github.com/marcoverpas/figures/blob/main/eco_plot_1.png" width="800">
</figure>

Note that the new **Figure 1** and **Figure 2** demonstrate that the behavior of the economy is qualitatively unchanged, except for the effect of the rise in temperature on consumption.  

## B5_Experiments

A simple experiment is to increase the (negative) temperature elasticity of the propensity to consume out of income. More precisely, it is assumed here that $\alpha_{12}$ quadruples (see the complete model code [here](https://github.com/marcoverpas/EAEPE_summer_school_2024/blob/main/eaepe_eco_model_plus.R)).  

<figure>
<img
src="https://github.com/marcoverpas/figures/blob/main/eco_plot_2.png" width="800">
</figure>

The figures above show that, while a higher sensitivity of consumption to an increase in temperature leads to an initial reduction in economic activity and environmental impacts, the situation reverses in the long run. This is because the private sector accumulates a higher stock of government bills, which, in turn, implies a higher flow of interest payments and thus a higher disposable income in the long run. Note that the level of the interest rate becomes a crucial variable for the determination of such rebound effects. 

## B6_Suggested_readings

1. *The cookbook of SFC economic theory and modelling*: Godley W. and Lavoie M. (2007) [Monetary Economics. An Integrated Approach to Credit, Money, Income, Production and Wealth](https://link.springer.com/book/10.1007/978-1-137-08599-3), Palgrave Macmillan, Basingstoke.

1. *A key reference for ecological SFC macro models*: Dafermos Y., Nikolaidi M. and Galanis G. (2017) [A stock-flow-fund ecological macroeconomic model](https://www.sciencedirect.com/science/article/pii/S0921800916301343), Ecological Economics, 131(1), pp. 191-207.

1. *A survey on SFC literature*: Nikiforos M. and Zezza G. (2017) [Stock-flow macroeconomic models: a survey](https://onlinelibrary.wiley.com/doi/10.1111/joes.12221), Journal of Economic Surveys, 31(5), pp. 1204-1239. 

1. *A simple IO-SFC model with bank money*: Veronese Passarella M. (2023) [Technical change and the monetary circuit: an input-output stock-flow consistent dynamic model](https://www.deps.unisi.it/sites/st02/files/allegatiparagrafo/07-09-2023/903.pdf), Dipartimento di Economia Politica e Statistica – Università di Siena, WP No. 903.
